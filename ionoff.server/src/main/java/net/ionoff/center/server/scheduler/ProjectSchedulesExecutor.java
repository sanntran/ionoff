package net.ionoff.center.server.scheduler;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import net.ionoff.center.server.control.IControlService;
import net.ionoff.center.server.control.UnknownRelayDriverModelException;
import net.ionoff.center.server.entity.Device;
import net.ionoff.center.server.entity.Mode;
import net.ionoff.center.server.entity.Schedule;
import net.ionoff.center.server.entity.ScheduleAction;
import net.ionoff.center.server.entity.SchedulePlayerAction;
import net.ionoff.center.server.entity.ScheduleRelayAction;
import net.ionoff.center.server.persistence.service.IModeService;
import net.ionoff.center.server.persistence.service.IScheduleService;
import net.ionoff.center.server.driver.api.RelayDriverException;
import net.ionoff.center.shared.dto.ScheduleConst;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayerConnectException;

@Component
@EnableAsync
@EnableScheduling
public class ProjectSchedulesExecutor {

	private static Logger LOGGER = Logger.getLogger(ProjectSchedulesExecutor.class.getName());
	
	private final SimpleDateFormat scheduleTimeFormat;
	private final SimpleDateFormat scheduleDayFormat; 
	private final SimpleDateFormat scheduleDateFormat;
	private final SimpleDateFormat simpleDateTimeFormat;
	
	private static final int INTERVAL = 60000; // 1 minutes
	
	private IScheduleService scheduleService;
	private IModeService modeService;
	private IControlService controlService;
	
	@Autowired
	public ProjectSchedulesExecutor(IScheduleService scheduleService, 
			IModeService modeService, IControlService controlService) {
		
		this.controlService = controlService;
		this.modeService = modeService;
		this.scheduleService = scheduleService;
		
		scheduleTimeFormat = new SimpleDateFormat("hh:mm a");
		scheduleDateFormat = new SimpleDateFormat("yyyy-MM-dd");
		scheduleDayFormat = new SimpleDateFormat("EEE");
		simpleDateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss a");
	}
	
	@Scheduled(fixedRate = INTERVAL)
    public void findAndExecuteSchedules() {
		Date now = new Date();
		findAndExecuteModes(now);
		findAndExecuteSchedules(now);
	}

	private void findAndExecuteModes(Date now) {
		List<Mode> executableModes = getExecutableModes(now);
		for (Mode mode : executableModes) {
			doExecuteMode(mode, now);
		}
	}

	@Async
	private void doExecuteMode(Mode mode, Date now) {
		LOGGER.info("Executing mode: " + mode.getNameId() + " at " + simpleDateTimeFormat.format(now));
		controlService.activateMode(mode);
	}

	private List<Mode> getExecutableModes(Date now) {
		String hhMMAmPm = scheduleTimeFormat.format(now);
		List<Mode> executableModes = modeService.findByScheduleTime(hhMMAmPm);
		
		List<Mode> todayExcutableModes = new ArrayList<Mode>();
		for (Mode mode : executableModes) {
			if (isTodaySchedule(mode.getScheduleRepeat(), mode.getScheduleDay(), now)) {
				todayExcutableModes.add(mode);
			}
		}
		return executableModes;
	}

	private void findAndExecuteSchedules(Date now) {
		executeFailedSchedules(getFailedSchedules());
		executeSchedules(getExecutableSchedules(now), now);
	}

	private void executeSchedules(List<Schedule> executableSchedules, Date now) {
		for (Schedule schedule : executableSchedules) {
			LOGGER.info("Execute schedules at " + simpleDateTimeFormat.format(now));
			executeSchedule(schedule);
		}
	}

	private List<Schedule> getExecutableSchedules(Date now) {
		String hhMMAmPm = scheduleTimeFormat.format(now);
		List<Schedule> excutableSchedules = scheduleService.findEnabledSchedules(hhMMAmPm);
		List<Schedule> todayExcutableSchedules = new ArrayList<Schedule>();
		for (Schedule schedule : excutableSchedules) {
			if (isTodaySchedule(schedule.getRepeat(), schedule.getDay(), now)) {
				todayExcutableSchedules.add(schedule);
			}
		}
		return excutableSchedules;
	}

	private boolean isTodaySchedule(String scheduleRepeat, String scheduleDay, Date now) {
		if (ScheduleConst.REPEAT_DAILY.equalsIgnoreCase(scheduleRepeat)) {
			return true;
		}
		if (ScheduleConst.REPEAT_WEEKLY.equalsIgnoreCase(scheduleRepeat)) {
			String day = scheduleDayFormat.format(now);
			return scheduleDay.contains(day);
		}
		if (ScheduleConst.REPEAT_ONCE.equalsIgnoreCase(scheduleRepeat)) {
			String today = scheduleDateFormat.format(now);
			return today.equalsIgnoreCase(scheduleDay);
		}
		return false;
	}

	private List<Schedule> getFailedSchedules() {
		List<Schedule> failedSchedules = scheduleService.findFailedSchedules();
		List<Schedule> result = new ArrayList<Schedule>();
		for (Schedule schedule : failedSchedules) {
			if (schedule.getRetry() == null || schedule.getRetry().intValue() < 3) {
				result.add(schedule);
			}
		}
		return result;
	}

	private void executeFailedSchedules(List<Schedule> failedSchedules) {
		long now = System.currentTimeMillis();
		for (Schedule schedule : failedSchedules) {
			executeFailedSchedule(schedule, now);
		}
	}
	
	@Async
	private void executeFailedSchedule(Schedule schedule, long now) {
		LOGGER.info("Retry to execute scheduler, id: " + schedule.getId()
				+ ", device: " + schedule.getDevice().getName());
		if (isDeviceControlledAfterExecutingSchedule(schedule.getDevice(), schedule)) {
			LOGGER.info("The device is controlled after executing the scheduler. The scheduler is ignored");
			updateSchedule(schedule, now, true, 0);
			return;
		}
		try {
			executeScheduleActions(schedule);
			updateSchedule(schedule, now, true, 0);
		}
		catch (Exception e) {
			LOGGER.error(e);
			Integer retry = schedule.getRetry();
			if (retry == null) {
				retry = 1;
			}
			else {
				retry = retry + 1;
			}
			updateSchedule(schedule, now, false, retry);
		}
	}

	private void updateSchedule(Schedule schedule, long time, boolean status, int retry) {
		schedule.setExecutedTime(time);
		schedule.setStatus(true);
		schedule.setRetry(0);
		scheduleService.update(schedule);
	}

	private boolean isDeviceControlledAfterExecutingSchedule(Device device, Schedule schedule) {
		return device.getTime() != null && schedule.getExecutedTime() != null
				&& device.getTime().getTime() > schedule.getExecutedTime().longValue();
	}

	@Async
	private void executeSchedule(Schedule schedule) {
		LOGGER.info("Excuting scheduler, id: " + schedule.getId() + ", device: " + schedule.getDevice().getName());
		long now = System.currentTimeMillis();
		try {
			executeScheduleActions(schedule);
			updateSchedule(schedule, now, true, 0);
		}
		catch (PlayerConnectException | DataServiceException | RelayDriverException | UnknownRelayDriverModelException e) {
			LOGGER.error(e.getMessage(), e);
			updateSchedule(schedule, now, false, 0);
		}
	}

	private void executeScheduleActions(Schedule schedule) throws PlayerConnectException, 
						RelayDriverException, DataServiceException, UnknownRelayDriverModelException {
		if (schedule.getActions() == null) {
			return;
		}
		for (ScheduleAction scheduleAction : schedule.getActions()) {
			if (scheduleAction instanceof SchedulePlayerAction) {
				executeSchedulePlayerAction((SchedulePlayerAction)scheduleAction);
			}
			else if (scheduleAction instanceof ScheduleRelayAction) {
				executeScheduleRelayAction((ScheduleRelayAction)scheduleAction);
			}
		}
	}

	private void executeScheduleRelayAction(ScheduleRelayAction scheduleAction) throws RelayDriverException, UnknownRelayDriverModelException {
		controlService.executeRelayAction(scheduleAction.getRelay(), scheduleAction.getAction());
	}

	private void executeSchedulePlayerAction(SchedulePlayerAction scheduleAction) throws PlayerConnectException, DataServiceException {
		controlService.executePlayerAction(scheduleAction.getPlayer(), scheduleAction.getAction(),
				scheduleAction.getVolume(), scheduleAction.getAlbum(), scheduleAction.getAlbumType());
	}
}

