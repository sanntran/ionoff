package net.ionoff.center.client.mode;

import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCheckBox;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialTab;
import gwt.material.design.client.ui.MaterialTabItem;
import net.ionoff.center.client.common.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.schedule.ScheduleTimeSettingPanel;
import net.ionoff.center.shared.dto.ModeDto;

public class ModeEditView extends AbstractEditView<ModeDto> implements ModeEditPresenter.Display {
	
	private final MaterialCheckBox checkBoxIsScheduled;
	private final ScheduleTimeSettingPanel scheduleTimeSettingPanel;
	private final Label lblSetting;
	private final MaterialTab tabBar;
	private final MaterialLink sceneTabLink;
	private final MaterialLink sensorTabLink;
	private final FlowPanel settingTabContentWrapper;
	private final ModeSceneListView scenesTabContent;
	private final ModeSensorsSettingView sensorTabContent;
	
	private final MaterialTabItem sceneTabItem;
	private final MaterialTabItem sensorTabItem;
	
	public ModeEditView() {
		super();
		getLblIcon().setIconType(IconType.SETTINGS_BRIGHTNESS);
		
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
		
		lblSetting = new InlineLabel(AdminLocale.getAdminConst().setting());
		lblSetting.setStyleName("lbl");
		contentPanel.add(lblSetting);
		
		tabBar = new MaterialTab();
		tabBar.setBackgroundColor(Color.GREY_LIGHTEN_3);
		tabBar.setIndicatorColor(Color.RED);
		contentPanel.add(tabBar);
		
		sceneTabItem = new MaterialTabItem();
		sceneTabItem.setGrid("s6");
		tabBar.add(sceneTabItem);
		
		sceneTabLink = new MaterialLink();
		sceneTabLink.setHref("#modeSceneTab");
		sceneTabLink.setText(AdminLocale.getAdminConst().scene());
		sceneTabItem.add(sceneTabLink);
		
		sensorTabItem = new MaterialTabItem();
		sensorTabItem.setGrid("s6");
		tabBar.add(sensorTabItem);
		
	    sensorTabLink = new MaterialLink();
	    sensorTabLink.setHref("#modeSensorTab");
		sensorTabLink.setText(AdminLocale.getAdminConst().sensor());
		sensorTabItem.add(sensorTabLink);
		
		settingTabContentWrapper = new FlowPanel();
		contentPanel.add(settingTabContentWrapper);
		
		scenesTabContent = new ModeSceneListView();
		sensorTabContent = new ModeSensorsSettingView();
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
	public MaterialLink getSensorSettingTab() {
		return sensorTabLink;
	}
	
	@Override
	public MaterialLink getSceneSettingTab() {
		return sceneTabLink;
	}
	
	@Override
	public FlowPanel getSettingTabContentWrapper() {
		return settingTabContentWrapper;
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
	public void hideSettingTabPanel() {
		lblSetting.setVisible(false);
		tabBar.setVisible(false);
		settingTabContentWrapper.setVisible(false);
	}

	@Override
	public void showSettingTabPanel() {
		lblSetting.setVisible(true);
		tabBar.setVisible(true);
		settingTabContentWrapper.setVisible(true);
		
	}
	
	@Override
	public ModeSceneListView getSceneTabContent() {
		return scenesTabContent;
	}
	
	@Override
	public ModeSensorsSettingView getSensorTabContent() {
		return sensorTabContent;
	}
	
	@Override
	public MaterialTab getSettingTabBar() {
		return tabBar;
	}
	
	@Override
	public MaterialTabItem getSceneTabItem() {
		return sceneTabItem;
	}
	
	@Override
	public MaterialTabItem getSensorTabItem() {
		return sensorTabItem;
	}
}
