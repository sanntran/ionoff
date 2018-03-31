package net.ionoff.center.client;

import net.ionoff.center.shared.dto.ProjectDto;

/**
 * @author Sann Tran
 */

public interface IAppController {
	
	void showLoading(boolean loading);

	void logout();

	void onClickLogo();

	void showLogin();

	void changeLanguage(String language);
	
	void showMain();
	
	void showModes();
	
	void showDashboard();

	void showPlayer();

	void showDevices();

	void showScenes();
	
	void showSchedules();

	void showControllers();

	void showRelays();

	void showUsers();

	void showSensors();

	void showAreas();

	void showZones();

	void showProjects();

	void showSystemMenu();

	void showProjectMenu();

	void showZoneMenu();
	
	void changeProject(ProjectDto project);

}
