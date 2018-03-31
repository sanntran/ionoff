package net.ionoff.center.client;

import net.ionoff.center.shared.dto.ProjectDto;



/**
 * @author Sann Tran
 */

public interface IClientAppController {

	public void go(ProjectDto project);

	public void showLoading(boolean loading);

	public void logout();

	public void onClickLogo();

	public void showLogin();

	public void changeLanguage(String language);

}
