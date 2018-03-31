package net.ionoff.center.client.dashboard;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCardTitle;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import net.ionoff.center.client.locale.ProjectLocale;

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
	MaterialCard cartDevice;
	
	@UiField
	MaterialCardTitle cartDeviceTitle;
	@UiField
	MaterialLabel lblTotalDevice;
	@UiField
	MaterialLabel lblDeviceOn;
	@UiField
	MaterialLabel lblDeviceOff;
	
	@UiField
	MaterialCard cartMode;
	@UiField
	MaterialCardTitle cartModeTitle;
	@UiField
	MaterialLabel lblTotalMode;
	@UiField
	MaterialLabel lblActivatedMode;
	
	@UiField
	MaterialCard cartController;
	@UiField
	MaterialCardTitle cartControllerTitle;
	@UiField
	MaterialLabel lblTotalController;
	@UiField
	MaterialLabel lblControllerOnline;
	@UiField
	MaterialLabel lblControllerOffline;
	
	@UiField
	MaterialCard cartSchedule;
	@UiField
	MaterialCardTitle cartScheduleTitle;
	@UiField
	MaterialLabel lblTotalSchedule;
	@UiField
	MaterialLabel lblNextSchedule;
	
	@UiField
	MaterialCard cartScene;
	@UiField
	MaterialCardTitle cartSceneTitle;
	@UiField
	MaterialLabel lblTotalScene;
	@UiField
	MaterialLabel lblLastTriggeredScene;

	@UiField
	MaterialCard cartServerChart;
	
	private DashboardChartView serverChart;
	
	@UiField
	FlowPanel deviceWrapper;

	public DashboardView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(ProjectLocale.getProjectConst().dashboard());
		cartDeviceTitle.setText(ProjectLocale.getProjectConst().device());
		cartControllerTitle.setText(ProjectLocale.getProjectConst().controller());
		cartModeTitle.setText(ProjectLocale.getProjectConst().mode());
		cartScheduleTitle.setText(ProjectLocale.getProjectConst().schedule());
		cartSceneTitle.setText(ProjectLocale.getProjectConst().scene());
		
		serverChart = new DashboardChartView("", ProjectLocale.getProjectConst().ram());
		cartServerChart.add(serverChart);
	}

	@Override
	public Panel asPanel() {
		return this.root;
	}

	@Override
	public MaterialLabel getLblDeviceOn() {
		return lblDeviceOn;
	}

	@Override
	public MaterialLabel getLblDeviceOff() {
		return lblDeviceOff;
	}
	
	@Override
	public MaterialLabel getLblTotalDevice() {
		return lblTotalDevice;
	}

	@Override
	public MaterialLabel getLblTotalScene() {
		return lblTotalScene;
	}

	@Override
	public MaterialLabel getLblLastTriggeredScene() {
		return lblLastTriggeredScene;
	}
	
	@Override
	public MaterialLabel getLblTotalSchedule() {
		return lblTotalSchedule;
	}

	@Override
	public MaterialLabel getLblNextSchedule() {
		return lblNextSchedule;
	}

	@Override
	public MaterialLabel getLblTotalController() {
		return lblTotalController;
	}

	@Override
	public MaterialLabel getLblControllerOnline() {
		return lblControllerOnline;
	}

	@Override
	public MaterialLabel getLblControllerOffline() {
		return lblControllerOffline;
	}

	@Override
	public MaterialLabel getLblTotalMode() {
		return lblTotalMode;
	}

	@Override
	public MaterialLabel getLblActivatedMode() {
		return lblActivatedMode;
	}

	@Override
	public DashboardChartView getServerChart() {
		return serverChart;
	}
	
	@Override
	public MaterialCard getCartDevice() {
		return cartDevice;
	}
	
	@Override
	public MaterialCard getCartScene() {
		return cartScene;
	}
	
	@Override
	public MaterialCard getCartSchedule() {
		return cartSchedule;
	}
	
	@Override
	public MaterialCard getCartMode() {
		return cartMode;
	}
	
	@Override
	public MaterialCard getCartController() {
		return cartController;
	}
	
	@Override
	public MaterialCard getCartServerChart() {
		return cartServerChart;
	}

	@Override
	public FlowPanel getDeviceWrapper() {
		return deviceWrapper;
	}
	
}
