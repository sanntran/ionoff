package net.ionoff.center.client.user;

import com.google.gwt.user.client.ui.CheckBox;

import gwt.material.design.client.ui.MaterialCollectionItem;
import net.ionoff.center.shared.dto.UserSceneDto;

public class UserSceneView extends MaterialCollectionItem {

	private final CheckBox checkBoxRole;
	private final UserSceneDto userScene;

	public UserSceneView(UserSceneDto userScene) {
		this.userScene = userScene;
		addStyleName("userScene");
		checkBoxRole = new CheckBox(userScene.getSceneName());
		add(checkBoxRole);
		checkBoxRole.setValue(userScene.hasRole());
	}

	public UserSceneDto getUserScene() {
		return userScene;
	}

	public CheckBox getCheckBoxRole() {
		return this.checkBoxRole;
	}

	public int getCheckBoxValue() {
		return this.checkBoxRole.getValue() == true ? 1 : 0;
	}
}