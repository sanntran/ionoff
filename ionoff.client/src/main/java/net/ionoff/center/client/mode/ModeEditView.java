package net.ionoff.center.client.mode;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialIntegerBox;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.schedule.ScheduleTimeSettingPanel;
import net.ionoff.center.shared.dto.ModeDto;

public class ModeEditView extends AbstractEditView<ModeDto> implements ModeEditPresenter.Display {

	private MaterialIntegerBox intBoxOrder;
	private final MaterialCheckBox checkBoxIsScheduled;
	private final ScheduleTimeSettingPanel scheduleTimeSettingPanel;
	
	public ModeEditView() {
		super();
		getLblIcon().setIconType(IconType.SETTINGS_BRIGHTNESS);

		intBoxOrder = new MaterialIntegerBox();
		intBoxOrder.setMin("0");
		intBoxOrder.setLabel(AdminLocale.getAdminConst().order());
		intBoxOrder.setPlaceholder(AdminLocale.getAdminConst().order());
		contentPanel.add(intBoxOrder);

		checkBoxIsScheduled = new MaterialCheckBox();
		checkBoxIsScheduled.addStyleName("isScheduledCheckBox");
		checkBoxIsScheduled.setText(AdminLocale.getAdminConst().schedule());
		checkBoxIsScheduled.setValue(false);
		contentPanel.add(checkBoxIsScheduled);
		
		scheduleTimeSettingPanel = new ScheduleTimeSettingPanel();
		contentPanel.add(scheduleTimeSettingPanel);
		
		checkBoxIsScheduled.addValueChangeHandler(event -> checkShowingScheduleTimeSettingPanel(checkBoxIsScheduled.getValue()));
	}
	
	@Override
	public void checkShowingScheduleTimeSettingPanel(Boolean isScheduled) {
		scheduleTimeSettingPanel.setVisible(Boolean.TRUE.equals(isScheduled));
	}

	@Override
	public MaterialIntegerBox getIntBoxOrder() {
		return intBoxOrder;
	}

	@Override
	public MaterialCheckBox getCheckBoxScheduled() {
		return checkBoxIsScheduled;
	}
	
	@Override
	public ScheduleTimeSettingPanel getScheduleTimeSettingPanel() {
		return scheduleTimeSettingPanel;
	}

}
