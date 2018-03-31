package net.ionoff.center.client.schedule;

import java.util.Date;

import com.google.gwt.event.dom.client.BlurEvent;
import com.google.gwt.event.dom.client.BlurHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.i18n.client.DateTimeFormat;
import com.google.gwt.i18n.client.NumberFormat;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.ui.MaterialDatePicker;
import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.utils.CalendarUtil;
import net.ionoff.center.shared.dto.ScheduleConst;

public class ScheduleTimeSettingPanel extends FlowPanel {
	
	private MaterialListBox listBoxRepeats;
	private MaterialIntegerBox intBoxHour;
	private MaterialIntegerBox intBoxMinute;
	private MaterialListBox listBoxAmPm;
	private MaterialDatePicker datePicker;
	private final FlowPanel daysPanel;
	
	private final CheckBox checkBoxSun;
	private final CheckBox checkBoxMon;
	private final CheckBox checkBoxTue;
	private final CheckBox checkBoxWed;
	private final CheckBox checkBoxThu;
	private final CheckBox checkBoxFri;
	private final CheckBox checkBoxSat;
	
	private DateTimeFormat dateTimeFormat = DateTimeFormat.getFormat(ScheduleConst.DATE_FORMAT);
	
	public ScheduleTimeSettingPanel() {
		setStyleName("scheduleTimeSetting row");
		
		FlowPanel timePanel = new FlowPanel();
		timePanel.setHeight("70px");
		add(timePanel);
		
		intBoxHour = new MaterialIntegerBox();
		intBoxHour.addStyleName("col s4 no-padding");
		intBoxHour.setLabel(AdminLocale.getAdminConst().hour());
		timePanel.add(intBoxHour);
		
		intBoxMinute = new MaterialIntegerBox();
		intBoxMinute.addStyleName("col s4 no-padding");
		intBoxMinute.setLabel(AdminLocale.getAdminConst().minute());
		timePanel.add(intBoxMinute);
		
		FlowPanel amPmPanel = new FlowPanel();
		amPmPanel.addStyleName("col s4 no-padding amPm");
		timePanel.add(amPmPanel);
		
		listBoxAmPm = new MaterialListBox();
		listBoxAmPm.setPlaceholder(AdminLocale.getAdminConst().amPm());
		listBoxAmPm.addItem(CalendarUtil.AM);
		listBoxAmPm.addItem(CalendarUtil.PM);
		amPmPanel.add(listBoxAmPm);
		
		listBoxRepeats = new MaterialListBox();
		listBoxRepeats.setPlaceholder(AdminLocale.getAdminConst().repeat());
		listBoxRepeats.addItem(AdminLocale.getAdminConst().once());
		listBoxRepeats.addItem(AdminLocale.getAdminConst().weekly());
		listBoxRepeats.addItem(AdminLocale.getAdminConst().daily());
		add(listBoxRepeats);
		
		datePicker = new MaterialDatePicker();
		datePicker.setFormat("yyyy-mm-dd");
		datePicker.setPlaceholder(AdminLocale.getAdminConst().date());
		add(datePicker);
		
		daysPanel = new FlowPanel();
		daysPanel.setStyleName("panelDays");
		add(daysPanel);

		checkBoxSun = new CheckBox(AdminLocale.getAdminConst().sun());
		daysPanel.add(checkBoxSun);

		checkBoxMon = new CheckBox(AdminLocale.getAdminConst().mon());
		daysPanel.add(checkBoxMon);

		checkBoxTue = new CheckBox(AdminLocale.getAdminConst().tue());
		daysPanel.add(checkBoxTue);

		checkBoxWed = new CheckBox(AdminLocale.getAdminConst().wed());
		daysPanel.add(checkBoxWed);

		checkBoxThu = new CheckBox(AdminLocale.getAdminConst().thu());
		daysPanel.add(checkBoxThu);

		checkBoxFri = new CheckBox(AdminLocale.getAdminConst().fri());
		daysPanel.add(checkBoxFri);

		checkBoxSat = new CheckBox(AdminLocale.getAdminConst().sat());
		daysPanel.add(checkBoxSat);
		
		listBoxRepeats.addValueChangeHandler(new ValueChangeHandler<String>() {
			
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				if (listBoxRepeats.getSelectedIndex() == 0) {
					daysPanel.setVisible(false);
					datePicker.setVisible(true);
				}
				else {
					daysPanel.setVisible(true);
					datePicker.setVisible(false);
					if (listBoxRepeats.getSelectedIndex() == 1) {
						enableCheckboxDays();
					}
					else if (listBoxRepeats.getSelectedIndex() == 2) {
						disableCheckboxDays();
					}
				}
			}
		});
		
		intBoxHour.addBlurHandler(new BlurHandler() {
			@Override
			public void onBlur(BlurEvent event) {
				validateHourInput();
			}
		});
		
		intBoxMinute.addBlurHandler(new BlurHandler() {
			@Override
			public void onBlur(BlurEvent event) {
				validateMinuteInput();
			}
		});
	}
	
	private void validateMinuteInput() {
		if (intBoxMinute.getValue() == null || intBoxMinute.getValue().intValue() < 0) {
			intBoxMinute.setValue(0);
		}
		else if (intBoxMinute.getValue().intValue() > 59) {
			intBoxMinute.setValue(59);
		}
	}

	private void validateHourInput() {
		if (intBoxHour.getValue() == null || intBoxHour.getValue().intValue() < 1) {
			intBoxHour.setValue(1);
		}
		else if (intBoxHour.getValue().intValue() > 12) {
			intBoxHour.setValue(12);
		}
	}
	
	public void setScheduleData(String repeat, String day, String time) {
		if (ScheduleConst.REPEAT_ONCE.equals(repeat)) {
			listBoxRepeats.setSelectedIndex(0);
			try {
				Date date = dateTimeFormat.parse(day);
				datePicker.setValue(date);
			}
			catch (Exception e) {
				datePicker.setValue(null);
			}
			daysPanel.setVisible(false);
			datePicker.setVisible(true);
		}
		else {
			daysPanel.setVisible(true);
			datePicker.setVisible(false);
			if (ScheduleConst.REPEAT_WEEKLY.equals(repeat)) {
				listBoxRepeats.setSelectedIndex(1);
				enableCheckboxDays();
			}
			else if (ScheduleConst.REPEAT_DAILY.equals(repeat)) {
				listBoxRepeats.setSelectedIndex(2);
				disableCheckboxDays();
			}
			
			checkBoxSun.setValue(false);
			checkBoxMon.setValue(false);
			checkBoxTue.setValue(false);
			checkBoxWed.setValue(false);
			checkBoxThu.setValue(false);
			checkBoxFri.setValue(false);
			checkBoxSat.setValue(false);
			
			if (day == null) {
				return;
			}
			if (day.contains(CalendarUtil.SUN)) {
				checkBoxSun.setValue(true);
			}
			if (day.contains(CalendarUtil.MON)) {
				checkBoxMon.setValue(true);
			}
			if (day.contains(CalendarUtil.TUE)) {
				checkBoxTue.setValue(true);
			}
			if (day.contains(CalendarUtil.WED)) {
				checkBoxWed.setValue(true);
			}
			if (day.contains(CalendarUtil.THU)) {
				checkBoxThu.setValue(true);
			}
			if (day.contains(CalendarUtil.FRI)) {
				checkBoxFri.setValue(true);
			}
			if (day.contains(CalendarUtil.SAT)) {
				checkBoxSat.setValue(true);
			}
		}
		
		String amPm = CalendarUtil.getTimeAmPm(time);
		if (CalendarUtil.AM.equals(amPm)) {
			listBoxAmPm.setSelectedIndex(0);
		}
		else {
			listBoxAmPm.setSelectedIndex(1);
		}
		
		String min = CalendarUtil.getTimeMinute(time);
		intBoxMinute.setValue(Integer.parseInt(min));
		
		String hour = CalendarUtil.getTimeHour(time);
		intBoxHour.setValue(Integer.parseInt(hour));
	}
	
	public String getSelectedTime() {
		String time = "";
		if (intBoxHour.getValue() != null) {
			time = NumberFormat.getFormat("00").format(intBoxHour.getValue()); 
		}
		else {
			time = "00"; 
		}
		if (intBoxMinute.getValue() != null) {
			time = time + ":" + NumberFormat.getFormat("00").format(intBoxMinute.getValue()); 
		}
		else {
			time = time + ":00";
		}
		if (listBoxAmPm.getSelectedIndex() == 0) {
			time = time + " " + CalendarUtil.AM;
		}
		else {
			time = time + " " + CalendarUtil.PM;
		}
		return time;
	}
	
	public String getSelectedRepeat() {
		if (listBoxRepeats.getSelectedIndex() == 0) {
			return ScheduleConst.REPEAT_ONCE;
		}
		if (listBoxRepeats.getSelectedIndex() == 1) {
			return ScheduleConst.REPEAT_WEEKLY;
		}
		else {//(listBoxRepeats.getSelectedIndex() == 2) {
			return ScheduleConst.REPEAT_DAILY;
		}
	}
	
	public String getSelectedDay() {
		if (listBoxRepeats.getSelectedIndex() == 0) {
			Date date = datePicker.getValue();
			if (date != null) {
				return dateTimeFormat.format(date);
			}
			return "";
		}
		String days = "";
		if (checkBoxSun.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.SUN;
		}
		if (checkBoxMon.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.MON;
		}
		if (checkBoxTue.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.TUE;
		}
		if (checkBoxWed.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.WED;
		}
		if (checkBoxThu.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.THU;
		}
		if (checkBoxFri.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.FRI;
		}
		if (checkBoxSat.getValue().booleanValue()) {
			days = days + CalendarUtil.DAY_DLM + CalendarUtil.SAT;
		}
		if (days.startsWith(CalendarUtil.DAY_DLM)) {
			days = days.replaceFirst(CalendarUtil.DAY_DLM, "");
		}
		return days;
	}
	
	private void enableCheckboxDays() {
		checkBoxSun.setEnabled(true);
		checkBoxMon.setEnabled(true);
		checkBoxTue.setEnabled(true);
		checkBoxWed.setEnabled(true);
		checkBoxThu.setEnabled(true);
		checkBoxFri.setEnabled(true);
		checkBoxSat.setEnabled(true);
	}
	
	private void disableCheckboxDays() {
		checkBoxSun.setValue(true);
		checkBoxMon.setValue(true);
		checkBoxTue.setValue(true);
		checkBoxWed.setValue(true);
		checkBoxThu.setValue(true);
		checkBoxFri.setValue(true);
		checkBoxSat.setValue(true);
		
		checkBoxSun.setEnabled(false);
		checkBoxMon.setEnabled(false);
		checkBoxTue.setEnabled(false);
		checkBoxWed.setEnabled(false);
		checkBoxThu.setEnabled(false);
		checkBoxFri.setEnabled(false);
		checkBoxSat.setEnabled(false);
	}
}
