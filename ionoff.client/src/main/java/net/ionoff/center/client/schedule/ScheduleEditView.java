package net.ionoff.center.client.schedule;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCheckBox;
import net.ionoff.center.client.common.AbstractEditView;
import net.ionoff.center.client.common.DevicesSelectionPanel;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.ScheduleDto;

public class ScheduleEditView extends AbstractEditView<ScheduleDto> implements ScheduleEditPresenter.Display {
	
	private MaterialCheckBox checkBoxEnable;
	private ScheduleTimeSettingPanel scheduleTimeSettingPanel;
	private DevicesSelectionPanel devicesSelectionPanel;
	private ScheduleActionsView scheduleActionsView;
	
	public ScheduleEditView() {
		super();
		getLblIcon().setIconType(IconType.SCHEDULE);
		
		checkBoxEnable = new MaterialCheckBox();
		checkBoxEnable.addStyleName("checkBoxActive");
		checkBoxEnable.setText(AdminLocale.getAdminConst().enabled());
		checkBoxEnable.setValue(true);
		contentPanel.add(checkBoxEnable);
		
		scheduleTimeSettingPanel = new ScheduleTimeSettingPanel();
		contentPanel.add(scheduleTimeSettingPanel);
		
		devicesSelectionPanel = new DevicesSelectionPanel();
		contentPanel.add(devicesSelectionPanel);
		
		scheduleActionsView = new ScheduleActionsView();
		contentPanel.add(scheduleActionsView);
	}
	
	
	@Override
	public MaterialCheckBox getCheckBoxEnable() {
		return checkBoxEnable;
	}
	
	@Override
	public ScheduleTimeSettingPanel getScheduleTimeSettingPanel() {
		return scheduleTimeSettingPanel;
	}
	
	@Override
	public DevicesSelectionPanel getDevicesSelectionPanel() {
		return devicesSelectionPanel;
	}
	
	@Override
	public ScheduleActionsView getScheduleActionsView() {
		return scheduleActionsView;
	}
}
