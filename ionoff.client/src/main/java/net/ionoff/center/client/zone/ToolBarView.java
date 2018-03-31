package net.ionoff.center.client.zone;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.TextBox;

import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialListBox;

public class ToolBarView extends FlowPanel {
	
	private final TextBox textBoxKeyWord;
	private final MaterialListBox listBoxSearchBy;
	private final MaterialButton btnSearch;
	private final FlowPanel searchPanel;

	private final MaterialButton btnRefresh;
	private final MaterialButton btnAdd;
	private final MaterialButton btnRemove;
	private final MaterialButton btnCloseSearch;
	private final MaterialLink lblTitle;
	
	public ToolBarView() {
		setStyleName("toolbar");
		
		lblTitle = new MaterialLink();
		lblTitle.addStyleName("title");
		add(lblTitle);
		
		FlowPanel buttonsPanel = new FlowPanel();
		buttonsPanel.setStyleName("buttons");
		add(buttonsPanel);
		
		btnAdd = new MaterialButton();
		btnAdd.setIconType(IconType.ADD);
		btnAdd.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnAdd);
		
		btnRefresh = new MaterialButton();
		btnRefresh.setIconType(IconType.REFRESH);
		btnRefresh.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnRefresh);

		btnRemove = new MaterialButton();
		btnRemove.setIconType(IconType.REMOVE);
		btnRemove.setWaves(WavesType.LIGHT);
		buttonsPanel.add(btnRemove);

		searchPanel = new FlowPanel();
		searchPanel.setVisible(false);
		searchPanel.setStyleName("searchbar");
		add(searchPanel);
		
		textBoxKeyWord = new TextBox();
		searchPanel.add(textBoxKeyWord);

		listBoxSearchBy = new MaterialListBox();
		searchPanel.add(listBoxSearchBy);

		btnSearch = new MaterialButton();
		btnSearch.setIconType(IconType.SEARCH);
		btnSearch.setWaves(WavesType.LIGHT);
		btnSearch.addStyleName("search");
		buttonsPanel.add(btnSearch);
		
		btnCloseSearch = new MaterialButton(ButtonType.FLAT);
		btnCloseSearch.setIconType(IconType.CLOSE);
		btnCloseSearch.addStyleName("close");
		searchPanel.add(btnCloseSearch);
		
		btnCloseSearch.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				textBoxKeyWord.setText("");
				searchPanel.setVisible(false);
			}
		});
	}
	
	public MaterialLink getLblTitle() {
		return this.lblTitle;
	}
	
	public TextBox getTextBoxKeyWord() {
		return this.textBoxKeyWord;
	}

	public MaterialListBox getLisBoxSearchBy() {
		return this.listBoxSearchBy;
	}

	public MaterialButton getBtnSearch() {
		return this.btnSearch;
	}
	
	public FlowPanel getSearchPanel() {
		return this.searchPanel;
	}

	public MaterialButton getBtnRefresh() {
		return this.btnRefresh;
	}

	public MaterialButton getBtnAdd() {
		return this.btnAdd;
	}

	public MaterialButton getBtnRemove() {
		return this.btnRemove;
	}
}
