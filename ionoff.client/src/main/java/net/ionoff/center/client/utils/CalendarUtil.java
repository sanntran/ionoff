package net.ionoff.center.client.utils;

import java.util.ArrayList;
import java.util.List;

import net.ionoff.center.client.locale.AdminLocale;

public class CalendarUtil {
	public static final String DAY_DLM = ", ";
	public static final String DATE_DLM = "-";
	public static final String TIME_FORMAT = "hh:mm a";
	
	public static final String AM = "AM";
	public static final String PM = "PM";
	public static final String SUN = "Sun";
	public static final String MON = "Mon";
	public static final String TUE = "Tue";
	public static final String WED = "Wed";
	public static final String THU = "Thu";
	public static final String FRI = "Fri";
	public static final String SAT = "Sat";
	
	public static String getTimeAmPm(String scheduleTime) {
		if (scheduleTime == null) {
			return AM;
		}
		return scheduleTime.split(" ")[1];
	}
	
	public static String getTimeHour(String scheduleTime) {
		if (scheduleTime == null) {
			return "01";
		}
		return scheduleTime.split(":")[0];
	}
	
	public static String getTimeMinute(String scheduleTime) {
		if (scheduleTime == null) {
			return "00";
		}
		return scheduleTime.split(":")[1].split(" ")[0];
	}
	
	public static String formatScheduleTime(String scheduleTime, String scheduleDay) {
		if (scheduleTime == null) {
			scheduleTime = "";
		}
		if (scheduleDay == null || scheduleDay.isEmpty()) {
			return scheduleTime;
		}
		return scheduleTime + " [" + getDay(scheduleDay) + "]";
	}

	private static String getDay(String scheduleDay) {
		if (scheduleDay.contains(DATE_DLM)) {
			return scheduleDay;
		}
		
		List<String> days = new ArrayList<String>();
		String[] scheduleDays = scheduleDay.split(DAY_DLM);
		for (String day : scheduleDays) {
			if (SUN.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().sun());
			}
			else if (MON.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().mon());
			}
			else if (TUE.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().tue());
			}
			else if (WED.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().wed());
			}
			else if (THU.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().thu());
			}
			else if (FRI.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().fri());
			}
			else if (SAT.equalsIgnoreCase(day)) {
				days.add(AdminLocale.getAdminConst().sat());
			}
		}
		String result = "";
		for (int i = 0; i < days.size(); i++) {
			if (i == 0) {
				result = days.get(i);
			}
			else {
				result = result + ", " + days.get(i);
			}
		}
		return result;
	} 

}
