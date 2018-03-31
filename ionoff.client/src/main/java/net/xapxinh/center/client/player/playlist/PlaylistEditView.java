package net.xapxinh.center.client.player.playlist;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class PlaylistEditView extends Composite implements PlaylistEditPresenter.Display {

	@UiTemplate("PlaylistEditView.ui.xml")
	interface PlaylistEditViewUiBinder extends UiBinder<Widget, PlaylistEditView> {
	}
	private static PlaylistEditViewUiBinder uiBinder = GWT.create(PlaylistEditViewUiBinder.class);
	
	@UiField
	HTMLPanel root;

	@UiField
	Label lblName;
	
	@UiField
	Button btnSave;

	@UiField
	Button btnSaveAs;

	@UiField
	Button btnClose;

	@UiField
	Button btnPlay;
	
	@UiField
	Button btnDelete;

	@UiField
	Label lblTextBoxName;
	
	@UiField
	TextBox textBoxName;
	
	@UiField
	Label lblTextAreaDescription;
	
	@UiField
	TextArea textAreaDescription;
	
	@UiField
	FlowPanel nodesWrapper;

	public PlaylistEditView() {
		uiBinder.createAndBindUi(this);
		getBtnSaveAs().setVisible(false);
		lblTextBoxName.setText(PlayLocale.getPlayConsts().name());
		lblTextAreaDescription.setText(PlayLocale.getPlayConsts().description());
	}

	@Override
	public HTMLPanel asPanel() {
		return root;
	}
	
	@Override
	public Label getLblName() {
		return lblName;
	}
	
	@Override
	public Button getBtnSave() {
		return btnSave;
	}
	
	@Override
	public Button getBtnSaveAs() {
		return btnSaveAs;
	}
	
	@Override
	public Button getBtnClose() {
		return btnClose;
	}

	@Override
	public Button getBtnPlay() {
		return btnPlay;
	}

	@Override
	public Button getBtnDelete() {
		return btnDelete;
	}

	@Override
	public TextBox getTextBoxName() {
		return textBoxName;
	}

	@Override
	public TextArea getTextAreaDescription() {
		return textAreaDescription;
	}

	@Override
	public FlowPanel getNodesWrappers() {
		return nodesWrapper;
	}
}
