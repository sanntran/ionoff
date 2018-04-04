package net.xapxinh.player.thread;

import java.util.Date;

import org.apache.log4j.Logger;

import net.xapxinh.player.AppProperties;
import net.xapxinh.player.Application;
import net.xapxinh.player.model.Schedule;
import net.xapxinh.player.util.TimeUtil;

public class PlayerSchedulesExecutor extends Thread {

	private static Logger LOGGER = Logger.getLogger(PlayerSchedulesExecutor.class.getName());
	
	private static PlayerSchedulesExecutor instance;
	
	private static final int INTERVAL = 60000; // minutes
	private boolean isQuit;

	@Override
	public void run() {
		LOGGER.info("Player schedules executor thread has been started !");
		for (; true;) {
			try {
				if (isQuit) {
					return;
				}
				sleep(INTERVAL);
				executeSchedules();
			}
			catch (Throwable e) {
				LOGGER.error(e.getMessage(), e);
				if (e instanceof OutOfMemoryError) {
					System.gc();
				}
			}
		}
	}
	
	private void executeSchedules() {
		Date now = new Date();
		String dateTimeNow = TimeUtil.formatScheduleDateTime(now);
		Schedule schedule = AppProperties.getSchedule();
		if (!dateTimeNow.equals(schedule.getDateTime())) {
			return;
		}
		// this is the time to execute schedule action
		if (Schedule.NONE.equals(schedule.getAction())) {
			return;
		}
		if (Schedule.STOP.equals(schedule.getAction())) {
			Application.application().mediaPlayerPanel().stop();
			return;
		}
		if (Schedule.SHUTDOWN.equals(schedule.getAction())) {
			// shutdown code goes here
			LOGGER.info("Player is going to shutdown now. Byebye!");
		}
	}
	
	public static PlayerSchedulesExecutor getInstance() {
		if (instance == null) {
			instance = new PlayerSchedulesExecutor();
		}
		return instance;
	}

	public void quit() {
		getInstance().setQuit(true);
	}

	private void setQuit(boolean quit) {
		this.isQuit = quit;
	}
}

