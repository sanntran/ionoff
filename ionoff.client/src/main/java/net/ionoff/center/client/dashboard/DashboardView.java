package net.ionoff.center.client.dashboard;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.*;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
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
	MaterialRow cardRow;
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
	@UiField
	MaterialRow sliceRow;

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
	public MaterialRow getSliceRow() {
		return sliceRow;
	}

	@Override
	public MaterialLink getLblTitle() {
		return lblTitle;
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
	public void updateCardDevice(DeviceStatisticDto deviceStatistic) {
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
	public void updateCardMode(ModeStatisticDto modeStatistic) {
		if (modeStatistic == null) {
			cartMode.setTitleValue("0");
			cartMode.setDescription("");
		} else {
			cartMode.setTitleValue(modeStatistic.getTotalCount() + "");
			cartMode.setDescription(modeStatistic.getActivatedName());
		}
	}

	@Override
	public void updateCardScene(SceneStatisticDto sceneStatistic) {
		if (sceneStatistic == null) {
			cartScene.setTitleValue("0");
			cartScene.setDescription("");
		} else {
			cartScene.setTitleValue(sceneStatistic.getTotalCount() + "");
			cartScene.setDescription(sceneStatistic.getLastPlayedName());
		}
	}

	@Override
	public void updateCardSchedule(ScheduleStatisticDto scheduleStatistic) {
		if (scheduleStatistic == null) {
			cartSchedule.setTitleValue("0");
			cartSchedule.setDescription("");
		} else {
			cartSchedule.setTitleValue(scheduleStatistic.getTotalCount() + "");
			cartSchedule.setDescription(scheduleStatistic.getNextScheduleTime()  + ": " + scheduleStatistic.getNextScheduleName());
		}
	}

	@Override
	public void updateCardController(ControllerStatisticDto controllerStatistic) {
		if (controllerStatistic == null) {
			cartController.setTitleValue("0");
			cartController.setDetail("");
		} else {
			cartController.setTitleValue(controllerStatistic.getOfflineCount() + "");
			cartController.setDetail(controllerStatistic.getFirstOffline() == null ? "" : controllerStatistic.getFirstOffline().getName() + " ...");
		}
	}

	@Override
	public void updateCardSensor(SensorStatisticDto sensorStatistic) {
		if (sensorStatistic == null) {
			cartSensor.setTitleValue("0");
			cartSensor.setDetail("");
		} else {
			cartSensor.setTitleValue(sensorStatistic.getOfflineCount() + "");
			cartSensor.setDetail(sensorStatistic.getFirstOffline() == null ? "" : sensorStatistic.getFirstOffline().getName() + " ...");
		}
	}

	@Override
	public void updateCardAlert(AlertStatisticDto alertStatistic) {
		if (alertStatistic == null) {
			cartAlert.setTitleValue("0");
			cartAlert.setDetail("");
		} else {
			cartAlert.setTitleValue(alertStatistic.getTotalCount() + "");
			cartAlert.setDescription(alertStatistic.getFirstAlert() == null ? "" : alertStatistic.getFirstAlert().getName());
			cartAlert.setDetail(alertStatistic.getFirstAlert() == null ? "" : alertStatistic.getFirstAlert().getPathName() + " ...");
		}
	}

	@Override
	public void updateCardArea(AreaStatisticDto areaStatistic) {
		if (areaStatistic == null) {
			cartArea.setTitleValue("0");
			cartArea.setDetail("");
		} else {
			cartArea.setTitleValue(areaStatistic.getHavingAlertCount() + "");
			cartArea.setDetail(areaStatistic.getFirstHasAlert() == null ? "" : areaStatistic.getFirstHasAlert().getName() + " ...");
		}
	}

	@Override
	public void updateCardZone(ZoneStatisticDto zoneStatistic) {
		if (zoneStatistic == null) {
			cartZone.setTitleValue("0");
			cartZone.setDetail("");
		} else {
			cartZone.setTitleValue(zoneStatistic.getHavingAlertCount() + "");
			cartZone.setDetail(zoneStatistic.getFirstHasAlert() == null ? "" : zoneStatistic.getFirstHasAlert().getPathName() + " ...");
		}
	}
}
