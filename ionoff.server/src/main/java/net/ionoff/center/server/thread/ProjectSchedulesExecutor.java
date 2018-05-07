package net.ionoff.center.server.thread;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.apache.log4j.Logger;

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
import net.ionoff.center.server.relaydriver.api.RelayDriverException;
import net.ionoff.center.shared.dto.ScheduleConst;
import net.xapxinh.center.server.exception.DataServiceException;
import net.xapxinh.center.server.exception.PlayerConnectException;

public class ProjectSchedulesExecutor extends Thread {

	private static Logger LOGGER = Logger.getLogger(ProjectSchedulesExecutor.class.getName());
	
	private final SimpleDateFormat scheduleTimeFormat;
	private final SimpleDateFormat scheduleDayFormat; 
	private final SimpleDateFormat scheduleDateFormat;
	private final SimpleDateFormat simpleDateTimeFormat;
	
	private static final int INTERVAL = 1; // minutes
	
	private Long projectId;
	private IScheduleService scheduleService;
	private IModeService modeService;
	private IControlService controlService;
	
	public ProjectSchedulesExecutor(Long projectId, IScheduleService scheduleService, 
			IModeService modeService, IControlService controlService) {
		this.projectId = projectId;
		
		this.controlService = controlService;
		this.modeService = modeService;
		this.scheduleService = scheduleService;
		
		scheduleTimeFormat = new SimpleDateFormat("hh:mm a");
		scheduleDateFormat = new SimpleDateFormat("yyyy-MM-dd");
		scheduleDayFormat = new SimpleDateFormat("EEE");
		simpleDateTimeFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss a");
	}
	
	public void shutdown() {
		projectId = null;
		interrupt();
	} 

	@Override
	public void run() {
		LOGGER.info(getThreadName() + " Thread has been started !");
		for (; true;) {
			try {
				waitForExcutableTime();
				Date now = new Date();
				excuteModes(now);
				executeSchedules(now);
			}
			catch (ProjectNotFoundException e) {
				LOGGER.error(getThreadName() + " ProjectId is null. Thread " + getThreadName() + " is destroyed now!");
				return;
			}
			catch (Throwable e) {
				LOGGER.error(e.getMessage(), e);
				if (e instanceof OutOfMemoryError) {
					System.gc();
				}
			}
		}
	}
	
	private String getThreadName() {
		return "[Project #" + projectId + "]";
	}

	private void excuteModes(Date now) throws ProjectNotFoundException {
		executeModes(getExecutableModes(now), now);
	}

	private void executeModes(List<Mode> executableModes, Date now) {
		for (Mode mode : executableModes) {
			LOGGER.info(getThreadName() + " Executes modes at " + simpleDateTimeFormat.format(now));
			controlService.activateMode(mode);
		}
	}

	private List<Mode> getExecutableModes(Date now) throws ProjectNotFoundException {
		String hhMMAmPm = scheduleTimeFormat.format(now);
		List<Mode> executableModes = modeService.findByScheduleTime(getProjectId(), hhMMAmPm);
		
		List<Mode> todayExcutableModes = new ArrayList<Mode>();
		for (Mode mode : executableModes) {
			if (isTodaySchedule(mode.getScheduleRepeat(), mode.getScheduleDay(), now)) {
				todayExcutableModes.add(mode);
			}
		}
		return executableModes;
	}

	private void executeSchedules(Date now) throws ProjectNotFoundException {
		executeFailedSchedules(getFailedSchedules());
		executeSchedules(getExecutableSchedules(now), now);
	}

	private void executeSchedules(List<Schedule> executableSchedules, Date now) {
		for (Schedule schedule : executableSchedules) {
			LOGGER.info(getThreadName() + " Execute schedules at " + simpleDateTimeFormat.format(now));
			executeSchedule(schedule);
		}
	}

	private void waitForExcutableTime() {
		Calendar cal = Calendar.getInstance();
		int currentMinute = cal.get(Calendar.MINUTE);
		int currentSecond = cal.get(Calendar.SECOND);
		try {
			//logger.info(getThreadName() + " Current system time: " + simpleDateTimeFormat.format(cal.getTime()));
			int minutesSleep = INTERVAL - currentMinute % INTERVAL;
			if (minutesSleep != 0) {
				int ignoreSecond = 0;
				if (currentSecond > 1) {
					ignoreSecond = currentSecond - 1;
					//logger.info(getThreadName() + " Ignores " + ignoreSecond + " seconds in sleep time");
				}
				sleep(minutesSleep * 60000 - ignoreSecond * 1000); // sleep
			}
		}
		catch (Exception e) {
			LOGGER.error(e.getMessage(), e);
		}
	}

	private List<Schedule> getExecutableSchedules(Date now) throws ProjectNotFoundException {
		String hhMMAmPm = scheduleTimeFormat.format(now);
		List<Schedule> excutableSchedules = scheduleService.findEnabledSchedules(getProjectId(), hhMMAmPm);
		List<Schedule> todayExcutableSchedules = new ArrayList<Schedule>();
		for (Schedule schedule : excutableSchedules) {
			if (isTodaySchedule(schedule.getRepeat(), schedule.getDay(), now)) {
				todayExcutableSchedules.add(schedule);
			}
		}
		return excutableSchedules;
	}

	private long getProjectId() throws ProjectNotFoundException {
		if (projectId == null) {
			throw new ProjectNotFoundException();
		}
		return projectId;
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

	private List<Schedule> getFailedSchedules() throws ProjectNotFoundException{
		List<Schedule> failedSchedules = scheduleService.findFailedSchedules(getProjectId());
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
	
	private void executeFailedSchedule(Schedule schedule, long now) {
		LOGGER.info(getThreadName() + " Retry to execute schedule, id: " + schedule.getId() + ", device: " + schedule.getDevice().getName());
		if (isDeviceControlledAfterExecutingSchedule(schedule.getDevice(), schedule)) {
			LOGGER.info(getThreadName() + " The device is controlled after executing the schedule. The schedule is ignored");
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

	private void executeSchedule(Schedule schedule) {
		LOGGER.info(getThreadName() + " Excuting schedule, id: " + schedule.getId() + ", device: " + schedule.getDevice().getName());
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

