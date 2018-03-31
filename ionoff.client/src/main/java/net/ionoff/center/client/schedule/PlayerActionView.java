package net.ionoff.center.client.schedule;

import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.locale.AdminLocale;

public class PlayerActionView extends FlowPanel implements IPlayerActionView {
		
	private final MaterialListBox listBoxActions;
	
	private final FlowPanel albumSelectionPanel;
	private final MaterialIntegerBox intBoxVolume;
	private final MaterialTextBox textBoxAlbum;
	private final FlowPanel scrollPanel;

	public PlayerActionView() {
		
		setStyleName("playerAction");
			
		listBoxActions = createListBoxActions();
		listBoxActions.setStyleName("listBox action");
		add(listBoxActions);
		
		albumSelectionPanel = new FlowPanel();
		albumSelectionPanel.setStyleName("albumSelection");
		add(albumSelectionPanel);
		
		FlowPanel albumSettingPanel = new FlowPanel();
		albumSettingPanel.setStyleName("settingPanel");
		albumSelectionPanel.add(albumSettingPanel);
		
		intBoxVolume = new MaterialIntegerBox();
		intBoxVolume.addStyleName("col-xs-6 no-padding");
		intBoxVolume.setLabel(AdminLocale.getAdminConst().volume());
		albumSettingPanel.add(intBoxVolume);
		
		textBoxAlbum = new MaterialTextBox();
		textBoxAlbum.setLabel(AdminLocale.getAdminConst().album());
		textBoxAlbum.addStyleName("col-xs-6 no-padding");
		albumSettingPanel.add(textBoxAlbum);
		
		scrollPanel = new FlowPanel();
		scrollPanel.setStyleName("scrollPanel");
		albumSelectionPanel.add(scrollPanel);
	}

	private MaterialListBox createListBoxActions() {
		
		MaterialListBox actions = new MaterialListBox();
		actions.addItem(AdminLocale.getAdminConst().none());
		actions.addItem(AdminLocale.getAdminConst().play());
		actions.addItem(AdminLocale.getAdminConst().stop());

		return actions;
	}

	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public MaterialListBox getListBoxActions() {
		return listBoxActions;
	}
	
	@Override
	public MaterialIntegerBox getIntBoxVolume() {
		return intBoxVolume;
	}

	@Override
	public MaterialTextBox getTextBoxAlbum() {
		return textBoxAlbum;
	}
	
	@Override
	public FlowPanel getAlbumContainer() {
		return scrollPanel;
	}

	@Override
	public FlowPanel getAlbumSelectionPanel() {
		return albumSelectionPanel;
	}
}