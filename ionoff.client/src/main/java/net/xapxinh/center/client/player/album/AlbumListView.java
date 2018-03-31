package net.xapxinh.center.client.player.album;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.TextBox;

import gwt.material.design.client.ui.MaterialRow;
import net.xapxinh.center.client.player.locale.PlayLocale;

public class AlbumListView extends MaterialRow implements AlbumListPresenter.Display {

	private final Label lblTitle;
	private final TextBox textBoxKey;
	private final Button btnSearch;
	private final Button btnSearchMenu;
	private final Button btnSearchAll;
	private final Button btnSearchAlbum;
	private final Button btnSearchArtist;
	private final Button btnSearchAuthor;
	private final Button btnShowMore;
	private final Label lblMostListenAlbums;
	private final Label lblLatestAlbums;
	private final FlowPanel panelAlbums;
	private PopupPanel popupSearchMenu;

	public AlbumListView() {
		setStyleName("albums");

		final FlowPanel panelSearch = new FlowPanel();
		panelSearch.setStyleName("header");
		add(panelSearch);
		
		lblTitle = new Label(PlayLocale.getPlayConsts().album());
		panelSearch.add(lblTitle);
		lblTitle.addStyleName("title");

		textBoxKey = new TextBox();
		textBoxKey.addStyleName("textbox");
		panelSearch.add(textBoxKey);

		btnSearch = new Button("");
		btnSearch.setText("");
		panelSearch.add(btnSearch);
		btnSearch.addStyleName("search");
		
		btnSearchMenu = new Button("");
		panelSearch.add(btnSearchMenu);
		btnSearchMenu.addStyleName("searchMenu");
		
		panelAlbums = new FlowPanel();
		panelAlbums.setStyleName("albumList row");
		add(panelAlbums);

		lblMostListenAlbums = new Label(PlayLocale.getPlayConsts().mostListened());
		lblMostListenAlbums.setStyleName("groupTitle mostListened col s12");

		lblLatestAlbums = new Label(PlayLocale.getPlayConsts().latestUploaded());
		lblLatestAlbums.setStyleName("groupTitle latestUploaded col s12");

		// Paging
		btnShowMore = new Button(PlayLocale.getPlayConsts().showMore());
		panelAlbums.add(btnShowMore);
		btnShowMore.setStyleName("showMore");
		btnShowMore.setVisible(false);
		
		popupSearchMenu = new PopupPanel(true, true);
		popupSearchMenu.setStyleName("popup albumSearchMenu");
		
		FlowPanel container = new FlowPanel();
		container.setStyleName("wrapper");
		popupSearchMenu.setWidget(container);
		
		btnSearchAll = new Button(PlayLocale.getPlayConsts().all());
		container.add(btnSearchAll);
		btnSearchAll.addStyleName("item searchAll");

		btnSearchAlbum = new Button(PlayLocale.getPlayConsts().album());
		container.add(btnSearchAlbum);
		btnSearchAlbum.addStyleName("item searchAlbum");

		btnSearchArtist = new Button(PlayLocale.getPlayConsts().artist());
		container.add(btnSearchArtist);
		btnSearchArtist.addStyleName("item searchArtist");

		btnSearchAuthor = new Button(PlayLocale.getPlayConsts().author());
		container.add(btnSearchAuthor);
		btnSearchAuthor.addStyleName("item searchAuthor");
		
	}

	@Override
	public Panel asWidget() {
		return this;
	}

	@Override
	public FlowPanel getPanelAlbums() {
		return this.panelAlbums;
	}

	@Override
	public TextBox getTextBoxKey() {
		return this.textBoxKey;
	}

	@Override
	public Button getBtnSearch() {
		return this.btnSearch;
	}
	
	@Override
	public Button getBtnSearchMenu() {
		return this.btnSearchMenu;
	}
	
	@Override
	public Button getBtnSearchAll() {
		return this.btnSearchAll;
	}

	@Override
	public Button getBtnSearchAlbum() {
		return this.btnSearchAlbum;
	}

	@Override
	public Button getBtnSearchArtist() {
		return this.btnSearchArtist;
	}

	@Override
	public Button getBtnSearchAuthor() {
		return this.btnSearchAuthor;
	}

	@Override
	public Button getBtnShowMore() {
		return this.btnShowMore;
	}

	@Override
	public Label getLblMostListenCount() {
		return this.lblMostListenAlbums;
	}

	@Override
	public Label getLbllRecentlyUploaded() {
		return this.lblLatestAlbums;
	}
	
	@Override
	public PopupPanel getPopupSearchMenu() {
		return this.popupSearchMenu;
	}
}
