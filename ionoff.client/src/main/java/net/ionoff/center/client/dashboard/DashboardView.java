package net.ionoff.center.client.dashboard;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.*;
import gwt.material.design.client.ui.MaterialLink;
import net.ionoff.center.client.ui.DashboardCard;
import net.ionoff.center.shared.dto.*;

import static gwt.material.design.client.constants.Color.*;
import static gwt.material.design.client.constants.IconType.*;
import static net.ionoff.center.client.locale.ProjectLocale.getProjectConst;

public class DashboardView extends Composite implements DashboardPresenter.Display {

	@UiTemplate("DashboardView.ui.xml")
	interface DashboardViewUiBinder extends UiBinder<Widget, DashboardView> {
	}

	private static DashboardViewUiBinder uiBinder = GWT.create(DashboardViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	DashboardCard cartAlert;
	@UiField
	DashboardCard cartArea;
	@UiField
	DashboardCard cartZone;
	@UiField
	DashboardCard cartDevice;
	@UiField
	DashboardCard cartController;
	@UiField
	DashboardCard cartSensor;
	@UiField
	DashboardCard cartScene;
	@UiField
	DashboardCard cartSchedule;
	@UiField
	DashboardCard cartMode;

	private DashboardChartView serverChart;

	@UiField
	FlowPanel deviceWrapper;

	DashboardPresenter presenter;


	public DashboardView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(getProjectConst().dashboard());
		cartAlert.setTextColor(RED_LIGHTEN_5).setIconType(WARNING)
				.setName(getProjectConst().alert());
		cartArea.setTextColor(LIGHT_BLUE_ACCENT_1).setIconType(DOMAIN)
				.setName(getProjectConst().area())
				.setDescription(getProjectConst().hasAlert());
		cartZone.setTextColor(TEAL_ACCENT_1).setIconType(WEEKEND)
				.setName(getProjectConst().zone())
				.setDescription(getProjectConst().hasAlert());;
		cartDevice.setTextColor(TEAL_ACCENT_1).setIconType(DEVICES_OTHER)
				.setName(getProjectConst().device())
				.setDescription(getProjectConst().isOn());;
		cartController.setTextColor(GREEN_ACCENT_1).setIconType(MEMORY)
				.setName(getProjectConst().controller())
				.setDescription(getProjectConst().isOffline());;
		cartSensor.setTextColor(YELLOW_ACCENT_1).setIconType(WIFI_TETHERING)
				.setName(getProjectConst().sensor())
				.setDescription(getProjectConst().isOffline());;

		cartScene.setTextColor(CYAN_ACCENT_1).setIconType(SLIDESHOW)
				.setName(getProjectConst().scene());
		cartSchedule.setTextColor(ORANGE_ACCENT_1).setIconType(SCHEDULE)
				.setName(getProjectConst().schedule());
		cartMode.setTextColor(YELLOW_ACCENT_1).setIconType(SETTINGS_BRIGHTNESS)
				.setName(getProjectConst().mode());

	}


	@Override
	public Panel asPanel() {
		return this.root;
	}

	@Override
	public MaterialLink getLblTitle() {
		return lblTitle;
	}


	@Override
	public DashboardCard getCartDevice() {
		return cartDevice;
	}

	@Override
	public DashboardCard getCartController() {
		return cartController;
	}

	@Override
	public FlowPanel getDeviceWrapper() {
		return deviceWrapper;
	}

	public void setPresenter(DashboardPresenter presenter) {
		this.presenter = presenter;
		cartArea.setClickHandler(event -> presenter.onCardAreaClick());
		cartZone.setClickHandler(event -> presenter.onCardZoneClick());
		cartDevice.setClickHandler(event -> presenter.onCardDeviceClick());
		cartController.setClickHandler(event -> presenter.onCartControllerClick());
		cartSensor.setClickHandler(event -> presenter.onCartSensorClick());
	}


	@Override
	public void displayCartDeviceData(DeviceStatisticDto deviceStatistic) {
		if (deviceStatistic == null) {
			cartDevice.setTitleValue("0");
			cartDevice.setDetail("");
		} else {
			cartDevice.setTitleValue(deviceStatistic.getTotalOn() + "");
			DeviceDto deviceOn = deviceStatistic.getDeviceOn();
			String deviceOnDetail = deviceOn == null ? "" : deviceOn.getName() + " (" + deviceOn.getZoneName() + " / " + deviceOn.getAreaName() + ")...";
			cartDevice.setDetail(deviceOnDetail);
		}
	}

	@Override
	public void displayCartModeData(ModeStatisticDto modeStatistic) {
		if (modeStatistic == null) {
			cartMode.setTitleValue("0");
			cartMode.setDescription("");
		} else {
			cartMode.setTitleValue(modeStatistic.getTotalCount() + "");
			cartMode.setDescription(modeStatistic.getActivatedName());
		}
	}

	@Override
	public void displayCartSceneData(SceneStatisticDto sceneStatistic) {
		if (sceneStatistic == null) {
			cartScene.setTitleValue("0");
			cartScene.setDescription("");
		} else {
			cartScene.setTitleValue(sceneStatistic.getTotalCount() + "");
			cartScene.setDescription(sceneStatistic.getLastPlayedName());
		}
	}

	@Override
	public void displayCartScheduleData(ScheduleStatisticDto scheduleStatistic) {
		if (scheduleStatistic == null) {
			cartSchedule.setTitleValue("0");
			cartSchedule.setDescription("");
		} else {
			cartSchedule.setTitleValue(scheduleStatistic.getTotalCount() + "");
			cartSchedule.setDescription(scheduleStatistic.getNextScheduleTime()  + ": " + scheduleStatistic.getNextScheduleName());
		}
	}

}
