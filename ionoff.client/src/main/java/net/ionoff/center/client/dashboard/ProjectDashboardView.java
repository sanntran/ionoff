package net.ionoff.center.client.dashboard;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.*;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.client.ui.ProjectDashboardCard;
import net.ionoff.center.shared.dto.*;

import static gwt.material.design.client.constants.Color.*;
import static gwt.material.design.client.constants.IconType.*;
import static net.ionoff.center.client.locale.ProjectLocale.getProjectConst;

public class ProjectDashboardView extends Composite implements ProjectDashboardPresenter.Display {

	@UiTemplate("ProjectDashboardView.ui.xml")
	interface ProjectDashboardViewUiBinder extends UiBinder<Widget, ProjectDashboardView> {
	}
	static ProjectDashboardViewUiBinder uiBinder = GWT.create(ProjectDashboardViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialRow cardRow;
	@UiField
	ProjectDashboardCard cartAlert;
	@UiField
	ProjectDashboardCard cartArea;
	@UiField
	ProjectDashboardCard cartZone;
	@UiField
	ProjectDashboardCard cartDevice;
	@UiField
	ProjectDashboardCard cartController;
	@UiField
	ProjectDashboardCard cartSensor;
	@UiField
	ProjectDashboardCard cartScene;
	@UiField
	ProjectDashboardCard cartSchedule;
	@UiField
	ProjectDashboardCard cartMode;
	@UiField
	MaterialRow sliceRow;

	public ProjectDashboardView() {
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

	public void setPresenter(ProjectDashboardPresenter presenter) {
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
		int offlineCount = controllerStatistic == null ? 0 : controllerStatistic.getOfflineCount();
		String detail = controllerStatistic == null || controllerStatistic.getFirstOffline() == null
				? ""
				: controllerStatistic.getFirstOffline().getName() + " ...";
		cartController.setTitleValue(offlineCount + "");
		cartController.setDetail(detail);
		if (offlineCount > 0) {
			cartController.setBackgroundColor(ORANGE_DARKEN_3);
		} else {
			cartController.setBackgroundColor(BLUE_GREY_DARKEN_2);
		}
	}

	@Override
	public void updateCardSensor(SensorStatisticDto sensorStatistic) {
		int offlineCount = sensorStatistic == null ? 0 : sensorStatistic.getOfflineCount();
		String detail = sensorStatistic == null || sensorStatistic.getFirstOffline() == null
				? ""
				: sensorStatistic.getFirstOffline().getName() + " ...";
		cartSensor.setTitleValue(offlineCount + "");
		cartSensor.setDetail(detail);
		if (offlineCount > 0) {
			cartSensor.setBackgroundColor(ORANGE_DARKEN_3);
		} else {
			cartSensor.setBackgroundColor(BLUE_GREY_DARKEN_2);
		}
	}

	@Override
	public void updateCardAlert(AlertStatisticDto alertStatistic) {
		int count = alertStatistic == null ? 0 : alertStatistic.getTotalCount();
		String description = alertStatistic == null || alertStatistic.getFirstAlert() == null
				? ""
				: alertStatistic.getFirstAlert().getName();
		String detail = alertStatistic == null || alertStatistic.getFirstAlert() == null
				? ""
				: alertStatistic.getFirstAlert().getPathName() + " ...";
		cartAlert.setTitleValue(count + "");
		cartAlert.setDescription(description);
		cartAlert.setDetail(detail);
		if (count > 0) {
			cartAlert.setBackgroundColor(RED_ACCENT_2);
		} else {
			cartAlert.setBackgroundColor(BLUE_GREY_DARKEN_2);
		}
	}

	@Override
	public void updateCardArea(AreaStatisticDto areaStatistic) {
		int count = areaStatistic == null ? 0 : areaStatistic.getHavingAlertCount();
		String detail = areaStatistic == null || areaStatistic.getFirstHasAlert() == null
				? ""
				: areaStatistic.getFirstHasAlert().getName() + " ...";
		cartArea.setTitleValue(count + "");
		cartArea.setDetail(detail);
		if (count > 0) {
			cartArea.setBackgroundColor(RED_ACCENT_2);
		} else {
			cartArea.setBackgroundColor(BLUE_GREY_DARKEN_2);
		}
	}

	@Override
	public void updateCardZone(ZoneStatisticDto zoneStatistic) {
		int count = zoneStatistic == null ? 0 : zoneStatistic.getHavingAlertCount();
		String detail = zoneStatistic == null || zoneStatistic.getFirstHasAlert() == null
				? ""
				: zoneStatistic.getFirstHasAlert().getName() + " ...";
		cartZone.setTitleValue(count + "");
		cartZone.setDetail(detail);
		if (count > 0) {
			cartZone.setBackgroundColor(RED_ACCENT_2);
		} else {
			cartZone.setBackgroundColor(BLUE_GREY_DARKEN_2);
		}
	}
}
