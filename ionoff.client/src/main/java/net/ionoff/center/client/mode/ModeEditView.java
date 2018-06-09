package net.ionoff.center.client.mode;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

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
	private final ModeSceneListView sceneListView;
	
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
		
		checkBoxIsScheduled.addValueChangeHandler(new ValueChangeHandler<Boolean>() {
			@Override
			public void onValueChange(ValueChangeEvent<Boolean> event) {
				checkShowingScheduleTimeSettingPanel(checkBoxIsScheduled.getValue());
			}
		});
		
		Label lblAction = new InlineLabel(AdminLocale.getAdminConst().action());
		lblAction.setStyleName("lbl");
		contentPanel.add(lblAction);
				
		sceneListView = new ModeSceneListView();
		contentPanel.add(sceneListView);
	} 
	
	@Override
	public void checkShowingScheduleTimeSettingPanel(Boolean isScheduled) {
		if (isScheduled != null && isScheduled.booleanValue() == true) {
			scheduleTimeSettingPanel.setVisible(true);
		}
		else {
			scheduleTimeSettingPanel.setVisible(false);
		}
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
	
	@Override
	public ModeSceneListView getSceneListView() {
		return sceneListView;
	}
}
