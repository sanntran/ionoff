package net.ionoff.center.server.util;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

public class DateTimeUtil {

	public static final String DAY_DLM = "/";
	public static final String TIME_DLM = ":";
	public static final String HOUR = "Hour";
	public static final String MINUTE = "Minute";
	public static final String SECOND = "Second";
	public static final String DAY_OF_WEEK = "Day";
	public static final String AM = "AM";
	public static final String PM = "PM";
	public static final String AM_PM = "AmPm";
	public static final double timeZone = 7.0;
	public static String HHmmddMMyyyyFormatter = "HH/mm/dd/MM/yyyy";
	// Lunar - Solar date time
	public static final SimpleDateFormat yyyyMMddFormatter = new SimpleDateFormat("yyyy/MM/dd");
	public static final SimpleDateFormat ddMMHHmmFormatter = new SimpleDateFormat("MM/dd HH:mm");
	public static final SimpleDateFormat yyyyMMddHHmmFormatter = new SimpleDateFormat("yyyy/MM/dd HH:mm");
	public static final SimpleDateFormat yyyyMMdd_HHmmssFormatter =  new SimpleDateFormat("yyyyMMdd_HHmmss");
	public static final SimpleDateFormat yyyyMMddHHmmssFormatter = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss");


	/**
	 * @return empty list if error, else return a list of String contains date
	 *         and time with format: hour, minute, am-pm, day week, day month,
	 *         year.
	 */
	public static List<String> getCurrentSolarDateTime() {
		List<String> result = new ArrayList<String>();
		Calendar cal = Calendar.getInstance();
		SimpleDateFormat sdf = new SimpleDateFormat(HHmmddMMyyyyFormatter);
		String dateTime = sdf.format(cal.getTime());

		String dt[] = dateTime.split("/");

		if (dt.length != 5)
			return result;
		String amPm = AM;
		int hour = Integer.parseInt(dt[0]);
		if (hour >= 12) {
			if (hour > 12) {
				hour = hour - 12;
			}
			amPm = PM;
		}

		else if (hour == 0) {
			hour = 12;
		}

		result.add(hour + "");
		result.add(dt[1]);
		result.add(amPm);

		String day = cal.get(Calendar.DAY_OF_WEEK) + "";
		result.add(day);

		result.add(dt[2]);
		result.add(dt[3]);
		result.add(dt[4]);

		return result;
	}

	public static String getDateTime() {
		SimpleDateFormat sdfDate = new SimpleDateFormat("yyyy/MM/dd HH:mm");
		Date now = new Date();
		String strDate = sdfDate.format(now);
		return strDate;
	}	

	public static String getCurrentTimeStamp() {
		SimpleDateFormat sdfDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Date now = new Date();
		String strDate = sdfDate.format(now);
		return strDate;
	}
}
