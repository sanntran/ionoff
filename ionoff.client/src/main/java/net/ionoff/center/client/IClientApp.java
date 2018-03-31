package net.ionoff.center.client;

import net.ionoff.center.client.common.IPresenter;
import net.ionoff.center.shared.dto.ProjectDto;

/**
 * @author Sann Tran
 */
public interface IClientApp extends IPresenter {
	void showLogin();
	void showMain(ProjectDto project);
	void handleHistoryTokenChanged();
}
