package net.xapxinh.center.client.player.playlist;

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
import com.google.gwt.user.client.ui.TextBox;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.shared.dto.MessageDto;
import net.xapxinh.center.shared.dto.PlayListDto;

/**
 * @author Sann Tran
 */
public class PlaylistListPresenter extends AbstractPresenter {

	public interface Display {

		int PAGE_SIZE = 10;

		FlowPanel asWidget();

		FlowPanel getPanelPlaylists();
		
		FlowPanel getPanelMyPlaylists();
		
		FlowPanel getPanelAllPlaylists();

		TextBox getTextBoxKey();

		Button getBtnSearch();

		Button getBtnShowMore();

		Button getBtnMyPlaylistTab();

		Button getBtnAllPlaylistTab();

		PlaylistEditPresenter.Display getPlaylistEditView();
	}

	public static final String SEARCH_ALL = "all";
	public static final String SEARCH_TITLE = "title";
	public static final String SEARCH_SINGER = "singer";
	public static final String SEARCH_AUTHOR = "author";
	
	private final HandlerManager eventBus;
	private final PlayerPresenter playerPresenter;
	protected final Display display;
	private int pageNumber;
	private PlaylistEditPresenter playlistEditPresenter;
	
	
	public PlaylistListPresenter(HandlerManager eventBus, Display view, PlayerPresenter playerPresenter) {

		super(eventBus);
		this.eventBus = eventBus;
		this.display = view;
		this.playerPresenter = playerPresenter;
		pageNumber = 1;
	}

	public void bind() {
		
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
		
		display.getBtnMyPlaylistTab().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.getBtnMyPlaylistTab().addStyleName("selected");
				display.getBtnAllPlaylistTab().removeStyleName("selected");
				display.getPanelPlaylists().clear();
				display.getPanelPlaylists().add(display.getPanelMyPlaylists());
			}
		});
		
		display.getBtnAllPlaylistTab().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.getBtnMyPlaylistTab().removeStyleName("selected");
				display.getBtnAllPlaylistTab().addStyleName("selected");
				display.getPanelPlaylists().clear();
				display.getPanelPlaylists().add(display.getPanelAllPlaylists());
			}
		});
	}

	private void search() {
		setPageNumber(1);
		final String key = display.getTextBoxKey().getText();
		rpcSearchPlaylists(key);
	}

	protected void setPageNumber(int pageNumber) {
		this.pageNumber = pageNumber;
	}

	private void gotoShowMorePage() {
		setPageNumber(pageNumber + 1);
		final String key = display.getTextBoxKey().getText();
		rpcSearchPlaylists(key);
	}

	private void rpcGetMyPlaylists() {
		playerPresenter.getPlayerService().getMyPlaylists(playerPresenter.getPlayerId(), 
				new MethodCallback<List<PlayListDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, List<PlayListDto> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				showMyPlaylists(response);
			}
		});
	}

	public void showMyPlaylists(List<PlayListDto> playLists) {
		display.getPanelMyPlaylists().clear();
		display.getPanelPlaylists().add(display.getPanelMyPlaylists());
		for (final PlayListDto playlist : playLists) {
			final PlaylistView playlistView = new PlaylistView(playlist);
			final PlaylistPresenter playlistPresenter = new PlaylistPresenter(eventBus, playlist, playlistView, playerPresenter);
			playlistPresenter.go();
			display.getPanelMyPlaylists().add(playlistView);
			playlistView.getBtnDelete().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					rpcDeletePlaylist(playlist, playlistView);
				}
			});
			playlistView.getBtnExplore().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					getPlaylistEditPresenter().setPlaylist(playlist);
					getPlaylistEditPresenter().show(display.asWidget());
				}
			});
		}
	}

	protected PlaylistEditPresenter getPlaylistEditPresenter() {
		if (playlistEditPresenter == null) {
			playlistEditPresenter = new PlaylistEditPresenter(eventBus, display.getPlaylistEditView(), playerPresenter);
			display.asWidget().add(display.getPlaylistEditView().asPanel());
			playlistEditPresenter.go();
		}
		return playlistEditPresenter;
	}

	protected void rpcDeletePlaylist(PlayListDto playlist, final PlaylistView playlistView) {
		playerPresenter.getPlayerService().deletePlaylist(playlist.getId(), playerPresenter.getPlayerId(), 
				new MethodCallback<MessageDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, MessageDto response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				display.getPanelMyPlaylists().remove(playlistView);
			}
		});
	}

	public void showSearchResults(List<PlayListDto> playLists) {
		if (pageNumber == 1) {
			display.getPanelPlaylists().clear();
			display.getPanelAllPlaylists().clear();
		}
		if (playLists == null || playLists.isEmpty()) {
			return;
		}
		for (PlayListDto playlist : playLists) {
			final PlaylistView playlistView = new PlaylistView(playlist);
			final PlaylistPresenter playlistPresenter = new PlaylistPresenter(eventBus, playlist, playlistView, playerPresenter);
			playlistPresenter.go();
			display.getPanelMyPlaylists().add(playlistView);
		}

		if (playLists.size() >= 10) {
			display.getPanelAllPlaylists().add(display.getBtnShowMore());
			display.getBtnShowMore().setVisible(true);
		} else {
			display.getBtnShowMore().setVisible(false);
		}
	}


	private void rpcSearchPlaylists(String keySearch) {
		if (playerPresenter.getPlayerId() == null) {
			return;
		}
		playerPresenter.getPlayerService().getAllPlaylists(playerPresenter.getPlayerId(), keySearch, pageNumber, 
				Display.PAGE_SIZE, new MethodCallback<List<PlayListDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, List<PlayListDto> response) {
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
		if (display.getBtnMyPlaylistTab().getStyleName().contains("selected")) {
			rpcGetMyPlaylists();
		}
		else {
			// Not yet implemented
		}
	}

	public void reset() {
		pageNumber = 1;
		display.getTextBoxKey().setText("");
	}
}
