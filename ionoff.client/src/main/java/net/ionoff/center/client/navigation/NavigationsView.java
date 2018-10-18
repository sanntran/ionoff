package net.ionoff.center.client.navigation;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialImage;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialNavBar;
import gwt.material.design.client.ui.MaterialSideNavDrawer;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.locale.ProjectLocale;

public class NavigationsView extends Composite implements NavigationsPresenter.Display {

	@UiTemplate("NavigationsView.ui.xml")
	interface TopNavigationViewUiBinder extends UiBinder<Widget, NavigationsView> {
	}

	private static TopNavigationViewUiBinder uiBinder = GWT.create(TopNavigationViewUiBinder.class);

	@UiField
	HTMLPanel root;
	
	@UiField
	MaterialNavBar navBar;
	@UiField
	MaterialButton btnLogo;
	@UiField
	MaterialIcon dashhboardIcon;
	@UiField
	MaterialButton btnNavTitle;
	@UiField
	MaterialTitle navTitle;
	
	@UiField
	MaterialIcon btnUser;
	
	@UiField
	MaterialSideNavDrawer sideNav;
	
	@UiField
	MaterialImage btnImgProject;
	@UiField
	MaterialIcon iconSelectProject;
	@UiField
	MaterialIcon iconSystemSetting;
	@UiField
	MaterialLabel lblSystemTime;
	@UiField
	MaterialLabel lblSystemDate;
	@UiField 
	MaterialButton btnProfileTitle;
	@UiField 
	MaterialTitle profileTitle;
	
	@UiField
	MaterialLink menuItemProject;
	@UiField
	MaterialLink menuItemDashboard;
	@UiField
	MaterialLink menuItemDevice;
	@UiField
	MaterialLink menuItemScene;
	@UiField
	MaterialLink menuItemMode;
	@UiField
	MaterialLink menuItemSchedule;
	@UiField
	MaterialLink menuItemController;
	@UiField
	MaterialLink menuItemRelay;
	@UiField
	MaterialLink menuItemUser;
	@UiField
	MaterialLink menuItemSensor;
	@UiField
	MaterialLink menuItemArea;
	@UiField
	MaterialLink menuItemZone;
	@UiField
	MaterialLink menuItemArrow;
	
	PopupProjectsView popupProjectsView;

	public NavigationsView() {
		uiBinder.createAndBindUi(this);
		
		menuItemProject.setText(ProjectLocale.getProjectConst().project());
		menuItemDashboard.setText(ProjectLocale.getProjectConst().dashboard());
		menuItemDevice.setText(ProjectLocale.getProjectConst().device());
		menuItemScene.setText(ProjectLocale.getProjectConst().scene());
		menuItemMode.setText(ProjectLocale.getProjectConst().mode());
		menuItemSchedule.setText(ProjectLocale.getProjectConst().schedule());
		menuItemController.setText(ProjectLocale.getProjectConst().controller());
		menuItemRelay.setText(ProjectLocale.getProjectConst().relay());
		menuItemUser.setText(ProjectLocale.getProjectConst().user());
		menuItemSensor.setText(ProjectLocale.getProjectConst().sensor());
		menuItemArea.setText(ProjectLocale.getProjectConst().area());
		menuItemZone.setText(ProjectLocale.getProjectConst().zone());
		
		popupProjectsView = new PopupProjectsView();
		
	}
	
	@Override
	public MaterialNavBar getNavBar() {
		return navBar;
	}

	@Override
	public MaterialButton getNavBtnLogo() {
		return btnLogo;
	}
	
	@Override
	public MaterialIcon getNavIconDashhboard() {
		return dashhboardIcon;
	}

	@Override
	public MaterialButton getBtnNavTitle() {
		return btnNavTitle;
	}
	
	@Override
	public MaterialTitle getNavTitle() {
		return navTitle;
	}

	@Override
	public MaterialIcon getBtnUser() {
		return btnUser;
	}
	
	@Override
	public MaterialSideNavDrawer getSideNav() {
		return sideNav;
	}

	@Override
	public MaterialImage getBtnImgProject() {
		return btnImgProject;
	}
	
	@Override
	public MaterialTitle getProfileTitle() {
		return profileTitle;
	}
	
	@Override
	public MaterialIcon getIconSelectProject() {
		return iconSelectProject;
	}
	@Override
	public MaterialIcon getIconSystemSetting() {
		return iconSystemSetting;
	}

	@Override
	public MaterialLabel getLblSystemTime() {
		return lblSystemTime;
	}

	@Override
	public MaterialLabel getLblSystemDate() {
		return lblSystemDate;
	}
	
	@Override
	public MaterialButton getBtnProfileTitle() {
		return btnProfileTitle;
	}

	@Override
	public MaterialLink getMenuItemProject() {
		return menuItemProject;
	}
	
	@Override
	public MaterialLink getMenuItemDashboard() {
		return menuItemDashboard;
	}

	@Override
	public MaterialLink getMenuItemDevice() {
		return menuItemDevice;
	}
	
	@Override
	public MaterialLink getMenuItemScene() {
		return menuItemScene;
	}
	
	@Override
	public MaterialLink getMenuItemMode() {
		return menuItemMode;
	}

	@Override
	public MaterialLink getMenuItemSchedule() {
		return menuItemSchedule;
	}

	@Override
	public MaterialLink getMenuItemController() {
		return menuItemController;
	}

	@Override
	public MaterialLink getMenuItemRelay() {
		return menuItemRelay;
	}

	@Override
	public MaterialLink getMenuItemUser() {
		return menuItemUser;
	}

	@Override
	public MaterialLink getMenuItemSensor() {
		return menuItemSensor;
	}

	@Override
	public MaterialLink getMenuItemArea() {
		return menuItemArea;
	}

	@Override
	public MaterialLink getMenuItemZone() {
		return menuItemZone;
	}
	
	@Override
	public MaterialLink getMenuItemArrow() {
		return menuItemArrow;
	}
	
	@Override
	public PopupProjectsView getPopupProjectsView() {
		return this.popupProjectsView;
	}

	@Override
	public Panel asPanel() {
		return this.root;
	}

}
