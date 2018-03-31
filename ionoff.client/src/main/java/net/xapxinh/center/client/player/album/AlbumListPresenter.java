package net.xapxinh.center.client.player.album;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.dom.client.KeyCodes;
import com.google.gwt.event.dom.client.KeyUpEvent;
import com.google.gwt.event.dom.client.KeyUpHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.shared.dto.Album;

/**
 * @author Sann Tran
 */
public class AlbumListPresenter extends AbstractPresenter {

	public interface Display {

		int PAGE_SIZE = 10;

		Panel asWidget();

		FlowPanel getPanelAlbums();

		TextBox getTextBoxKey();

		Button getBtnSearch();

		Button getBtnShowMore();
		
		Button getBtnSearchMenu();

		Button getBtnSearchAll();

		Button getBtnSearchAlbum();

		Button getBtnSearchArtist();

		Button getBtnSearchAuthor();
		
		Label getLblMostListenCount();
		
		Label getLbllRecentlyUploaded();
		
		PopupPanel getPopupSearchMenu(); 
	}

	public static final String SEARCH_ALL = "all";
	public static final String SEARCH_TITLE = "title";
	public static final String SEARCH_SINGER = "singer";
	public static final String SEARCH_AUTHOR = "author";
	
	private final HandlerManager eventBus;
	private final PlayerPresenter playerPresenter;
	protected final Display display;
	private int pageNumber;
	private String searchScope;

	public AlbumListPresenter(HandlerManager eventBus, Display view, PlayerPresenter playerPresenter) {

		super(eventBus);
		this.eventBus = eventBus;
		this.display = view;
		this.playerPresenter = playerPresenter;
		pageNumber = 0;
	}

	public void bind() {
		
		searchScope = SEARCH_ALL;
		
		display.getTextBoxKey().addKeyUpHandler(new KeyUpHandler() {
			@Override
			public void onKeyUp(KeyUpEvent event) {
				if (event.getNativeKeyCode() == KeyCodes.KEY_ENTER) {
					search();
				}
			}
		});

		display.getBtnShowMore().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				gotoShowMorePage();
			}
		});

		display.getBtnSearch().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				search();
			}
		});
		display.getBtnSearchMenu().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (display.getPopupSearchMenu().isAttached()) {
					display.getPopupSearchMenu().hide();
				}
				else {
					Widget source = (Widget) event.getSource();
					int left = source.getAbsoluteLeft() + 30 - 160;
			        int top = source.getAbsoluteTop() + 41;
			        display.getPopupSearchMenu().setPopupPosition(left, top);
					display.getPopupSearchMenu().show();
				}
			}
		});
		
		display.getBtnSearchAll().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				removeSelected();
				display.getBtnSearch().setStyleName("gwt-Button search all selected");
				display.getBtnSearchAll().addStyleName("selected");
				searchScope = SEARCH_ALL;
				display.getPopupSearchMenu().hide();
				search();
			}
		});

		display.getBtnSearchAlbum().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				removeSelected();
				display.getBtnSearch().setStyleName("gwt-Button search album selected");
				display.getBtnSearchAlbum().addStyleName("selected");
				searchScope = SEARCH_TITLE;
				display.getPopupSearchMenu().hide();
				search();
			}
		});

		display.getBtnSearchArtist().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				removeSelected();
				display.getBtnSearch().setStyleName("gwt-Button search artist selected");
				display.getBtnSearchArtist().addStyleName("selected");
				searchScope = SEARCH_SINGER;
				display.getPopupSearchMenu().hide();
				search();
			}
		});

		display.getBtnSearchAuthor().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				removeSelected();
				display.getBtnSearch().setStyleName("gwt-Button search author selected");
				display.getBtnSearchAuthor().addStyleName("selected");
				searchScope = SEARCH_AUTHOR;
				display.getPopupSearchMenu().hide();
				search();
			}
		});
	}

	private void removeSelected() {
		display.getBtnSearch().removeStyleName("selected");;
		display.getBtnSearchAll().removeStyleName("selected");
		display.getBtnSearchAlbum().removeStyleName("selected");
		display.getBtnSearchArtist().removeStyleName("selected");
		display.getBtnSearchAuthor().removeStyleName("selected");
	}

	private void search() {
		setPageNumber(1);
		final String key = display.getTextBoxKey().getText();
		rpcSearchAlbums(key);
	}

	protected void setPageNumber(int pageNumber) {
		this.pageNumber = pageNumber;
	}

	private void gotoShowMorePage() {
		setPageNumber(pageNumber + 1);
		final String key = display.getTextBoxKey().getText();
		rpcSearchAlbums(key);
	}

	private void rpcGetSpecialAlbums() {
		playerPresenter.getPlayerService().getSpecialAlbum(playerPresenter.getPlayerId(), new MethodCallback<List<Album>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, List<Album> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				showSpecialAlbums(response);
			}
		});
	}

	public void showSpecialAlbums(List<Album> albums) {
		display.getPanelAlbums().clear();
		final int albumSize = albums.size();
		if (albumSize == 0 || albumSize < 10) {
			return;
		}
		//show 10 albums most listened count
		display.getPanelAlbums().add(display.getLblMostListenCount());
		for (int i = 0; i < 10; i++) {
			final Album album = albums.get(i);
			final AlbumItemView albumView = new AlbumItemView(album);
			final AlbumItemPresenter albumPresenter = new AlbumItemPresenter(eventBus, album, albumView, playerPresenter);
			albumPresenter.go();
			display.getPanelAlbums().add(albumView);
		}
		// show 10 albums recently uploaded
		display.getPanelAlbums().add(display.getLbllRecentlyUploaded());
		for (int i = 10; i < albumSize; i++) {
			final Album album = albums.get(i);
			final AlbumItemView albumView = new AlbumItemView(album);
			final AlbumItemPresenter albumPresenter = new AlbumItemPresenter(eventBus, album, albumView, playerPresenter);
			albumPresenter.go();
			display.getPanelAlbums().add(albumView);
		}
	}

	public void showSearchResults(List<Album> albums) {
		if (pageNumber == 1) {
			display.getPanelAlbums().clear();
		}
		if (albums == null || albums.isEmpty()) {
			return;
		}
		for (final Album album : albums) {
			final AlbumItemView albumView = new AlbumItemView(album);
			final AlbumItemPresenter albumPresenter = new AlbumItemPresenter(eventBus, album, albumView, playerPresenter);
			albumPresenter.go();
			display.getPanelAlbums().add(albumView);
		}

		final int countAlbum = albums.size();
		if (searchScope.equals(SEARCH_ALL)) {
			if (countAlbum >= 10) {
				display.getPanelAlbums().add(display.getBtnShowMore());
				display.getBtnShowMore().setVisible(true);
			} else {
				display.getBtnShowMore().setVisible(false);
			}
		}
		else {
			if (countAlbum == 10) {
				display.getPanelAlbums().add(display.getBtnShowMore());
				display.getBtnShowMore().setVisible(true);
			} else {
				display.getBtnShowMore().setVisible(false);
			}
		}

	}


	private void rpcSearchAlbums(String key) {
		if (playerPresenter.getPlayerId() == null) {
			return;
		}
		playerPresenter.getPlayerService().searchAlbums(playerPresenter.getPlayerId(), key, searchScope, pageNumber, 
				Display.PAGE_SIZE, new MethodCallback<List<Album>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, List<Album> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				showSearchResults(response);
			}
		});
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asWidget());
		if (pageNumber == 0) {
			rpcGetSpecialAlbums();
		}
	}

	public void setSearchBy(String searchBy) {
		this.searchScope = searchBy;
	}

	public void reset() {
		pageNumber = 0;
		display.getTextBoxKey().setText("");
	}
}
