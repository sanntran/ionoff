package net.xapxinh.center.client.player;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.HasWidgets;

import net.xapxinh.center.client.player.album.AlbumListPresenter;
import net.xapxinh.center.client.player.browse.FilesBrowsePresenter;
import net.xapxinh.center.client.player.event.MenuItemSelectedEvent;
import net.xapxinh.center.client.player.event.MenuItemSelectedEventHandler;
import net.xapxinh.center.client.player.event.RpcFailureEvent;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.client.player.event.ShowPlayerMenuEvent;
import net.xapxinh.center.client.player.event.ShowPlayerMenuEventHandler;
import net.xapxinh.center.client.player.menu.PlayerMenuPresenter;
import net.xapxinh.center.client.player.playing.PlayerControlPresenter;
import net.xapxinh.center.client.player.playlist.PlaylistListPresenter;
import net.xapxinh.center.client.player.rpc.PlayerService;
import net.xapxinh.center.client.player.token.PlayToken;
import net.xapxinh.center.client.player.youtube.YoutubeVideoListPresenter;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayerDto;
import net.xapxinh.center.shared.dto.Status;
import net.xapxinh.center.shared.dto.YoutubeVideos;

public class PlayerPresenter extends AbstractPresenter  {
	
	private static final int PLAYER_SYNC_STATUS = 4000; // 4 seconds

	private final IPlayerView display;
	private PlayerMenuPresenter menuInstance;
	private PlayerControlPresenter playingInstance;
	private FilesBrowsePresenter fileBrowseInstance;
	private AlbumListPresenter albumListInstance;
	private YoutubeVideoListPresenter youtubeVideoListInstance;
	private PlaylistListPresenter playlistListInstance;
	private final PlayerService playerService;
	private Timer syncStatusTimer;
	
	public PlayerPresenter(PlayerService playerService,
			HandlerManager eventBus, IPlayerView display) {
		super(eventBus);
		this.playerService = playerService;
		this.display = display;
	}

	protected Long playerId;

	public void rpcSendCommand(Command command) {
		rpcSendCommand(command, true);
	}

	public void rpcSendCommand(Command command, final boolean loadPlaylist) {
		if (playerId == null) {
			return;
		}
		getPlayerService().sendCommand(playerId, command, new MethodCallback<Status>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, Status response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				setStatus(response);
				setIconStyle(PlayerDto.STATUS.CONNECTED);
				if (loadPlaylist && getPlayingInstance().isShowingPlaylist()) {
					rpcGetPlaylist();
				}
			}
		});
	}

	public void getStatus() {
		getStatus(true);
	}

	public void getStatus(final boolean loadPlaylist) {
		if (playerId == null) {
			return;
		}
		getPlayerService().getStatus(playerId, new MethodCallback<Status>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, Status response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				setStatus(response);
				setIconStyle(PlayerDto.STATUS.CONNECTED);
				if (loadPlaylist && getPlayingInstance().isShowingPlaylist()) {
					rpcGetPlaylist();
				}
			}
		});
	}

	private void setStatus(Status status) {
		getPlayingInstance().setStatus(status);
	}

	public void handleException(Method method, Throwable caught) {
		eventBus.fireEvent(new RpcFailureEvent(method, caught));
	}

	public void rpcGetPlaylist() {
		if (playerId == null) {
			return;
		}
		getPlayerService().getPlayingList(playerId, new MethodCallback<PlayListDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, PlayListDto response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				getPlayingInstance().setPlaylist(response);
			}
		});
	}

	public void cancelTimers() {
		if (syncStatusTimer != null) {
			syncStatusTimer.cancel();
			syncStatusTimer = null;
		}
	}

	protected void startSyncStatusTimer() {
		if (syncStatusTimer != null) {
			return;
		}
		syncStatusTimer = new Timer() {
			@Override
			public void run() {
				if (!PlayToken.hasTokenItem(PlayToken.PLAYER)) {
					cancelTimers();
				}
				getStatus();
			}
		};
		syncStatusTimer.scheduleRepeating(PLAYER_SYNC_STATUS);
	}

	public Long getPlayerId() {
		return playerId;
	}

	public PlayerService getPlayerService() {
		eventBus.fireEvent(new ShowLoadingEvent(true));
		return playerService;
	}

	protected YoutubeVideoListPresenter getYoutubeVideoListInstance() {
		if (youtubeVideoListInstance == null) {
			youtubeVideoListInstance = new YoutubeVideoListPresenter(eventBus, display.getYoutubeVideoListView(), this);
			youtubeVideoListInstance.go();
		}
		return youtubeVideoListInstance;
	}
	
	protected PlaylistListPresenter getPlaylistListInstance() {
		if (playlistListInstance == null) {
			playlistListInstance = new PlaylistListPresenter(eventBus, display.getPlaylistListView(), this);
			playlistListInstance.go();
		}
		return playlistListInstance;
	}

	public void bind() {
		getMenuInstance();
		eventBus.addHandler(MenuItemSelectedEvent.TYPE, new MenuItemSelectedEventHandler() {
			@Override
			public void onMenuItemSelected(MenuItemSelectedEvent event) {
				if (MenuItemSelectedEvent.ALBUM.equals(event.getMenuItem())) {
					showAlbumList();
				}
				else if (MenuItemSelectedEvent.BROWSE.equals(event.getMenuItem())) {
					showFileBrowse();
				}
				else if (MenuItemSelectedEvent.YOUTUBE.equals(event.getMenuItem())) {
					showYoutubeVideoList();
				}
				else if (MenuItemSelectedEvent.PLAYLIST.equals(event.getMenuItem())) {
					showPlaylistList();;
				}
			}
		});
		eventBus.addHandler(ShowPlayerMenuEvent.TYPE, new ShowPlayerMenuEventHandler() {
			@Override
			public void onShowPlayerMenu(ShowPlayerMenuEvent event) {
				if (display.getMenuView().isShowed()) {
					display.getMenuView().hide();
				}
				else {
					display.getMenuView().show();
				}
			}
		});
	}

	public void showAlbumList() {
		display.getMenuView().getAlbumMenuItem().setStyleName("menuItem selected");
		getAlbumListInstance().show(display.getCenterPanel());
	}

	public void showFileBrowse() {
		display.getMenuView().getFileMenuItem().setStyleName("menuItem selected");
		getFileBrowseInstance().show(display.getCenterPanel());
	}

	public void showYoutubeVideoList() {
		display.getMenuView().getYoutubeMenuItem().setStyleName("menuItem selected");
		getYoutubeVideoListInstance().show(display.getCenterPanel());
	}
	
	public void showPlaylistList() {
		display.getMenuView().getPlaylistMenuItem().setStyleName("menuItem selected");
		getPlaylistListInstance().show(display.getCenterPanel());
	}

	protected PlayerControlPresenter getPlayingInstance() {
		if (playingInstance == null) {
			playingInstance = new PlayerControlPresenter(eventBus, display.getPlayingView(), this);
			playingInstance.go();
		}
		return playingInstance;
	}

	public AlbumListPresenter getAlbumListInstance() {
		if (albumListInstance == null) {
			albumListInstance = new AlbumListPresenter(eventBus, display.getAlbumListView(), this);
			albumListInstance.setSearchBy(AlbumListPresenter.SEARCH_TITLE);
			albumListInstance.go();
		}
		return albumListInstance;
	}


	protected FilesBrowsePresenter getFileBrowseInstance() {
		if (fileBrowseInstance == null) {
			fileBrowseInstance = new FilesBrowsePresenter(eventBus, display.getFileBrowseView(), this);
			fileBrowseInstance.go();
		}
		return fileBrowseInstance;
	}

	protected PlayerMenuPresenter getMenuInstance() {
		if (menuInstance == null) {
			menuInstance = new PlayerMenuPresenter(eventBus, display.getMenuView());
			menuInstance.go();
		}
		return menuInstance;
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asPanel());
	}

	@Override
	public void go() {
		bind();
	}

	public void setPlayerId(Long playerId) {
		this.playerId = playerId;
		if (playerId == null) {
			cancelTimers();
		}
		else {
			startSyncStatusTimer();
		}
		if (playerId != null) {
			getStatus();
		}
		getMenuInstance().setDefaultMenuItemsStyle();
		getAlbumListInstance().reset();
		showAlbumList();
	}

	public void refreshMediaFiles(String path) {
		if (path == null) {
			path = "";
		}
		getPlayerService().refreshMediaFiles(playerId, path, new MethodCallback<List<MediaFile>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, List<MediaFile> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				getFileBrowseInstance().showMediaFiles(response);
			}
		});
	}
	
	public void getMediaFiles(String path) {
		if (path == null) {
			path = "";
		}
		getPlayerService().getMediaFiles(playerId, path, new MethodCallback<List<MediaFile>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, List<MediaFile> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				getFileBrowseInstance().showMediaFiles(response);
			}
		});
	}

	public void searchYoutubeVideo(final String key, String pageToken) {
		if (playerId == null) {
			return;
		}
		getPlayerService().searchYoutubeVideos(playerId, key, pageToken, new MethodCallback<YoutubeVideos>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, YoutubeVideos response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				getYoutubeVideoListInstance().showYoutubeVideos(response);
			}
		});
	}

	protected void setIconStyle(PlayerDto.STATUS status) {
		// does nothing
	}
}
