package net.xapxinh.center.client.player.album;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.Song;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Widget;

public class AlbumItemPresenter extends AbstractPresenter {

	public interface Display {
		Widget asWidget();

		Button getBtnTrackList();
		Button getBtnPlay();
		Button getBtnEnqueue();

		void showOrHidePanelTracks(List<Song> songs);
	}
	private final Display display;
	private final Album album;
	private final PlayerPresenter playerPresenter;

	public AlbumItemPresenter(HandlerManager eventBus, Album cdAlbum, Display view,
			PlayerPresenter playerPresenter) {
		super(eventBus);
		display = view;
		album = cdAlbum;
		this.playerPresenter = playerPresenter;
	}

	public void bind() {
		display.getBtnTrackList().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (album.getSongs() == null || album.getSongs().isEmpty()) {
					rpcLoadSongs();
				}
				else {
					display.showOrHidePanelTracks(album.getSongs());
				}
			}
		});
		display.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playAlbum(getAlbum());
			}
		});

		display.getBtnEnqueue().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				enqueueAlbum(getAlbum());
			}
		});
	}

	private void rpcLoadSongs() {
		if (playerPresenter.getPlayerId() == null) {
			return;
		}
		playerPresenter.getPlayerService().getAlbum(album.getId(), 
				playerPresenter.getPlayerId(), new MethodCallback<Album>() {
			
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, Album response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				album.setSongs(response.getSongs());
				display.showOrHidePanelTracks(album.getSongs());
			}
		});
	}

	private void playAlbum(Album album) {
		playerPresenter.rpcSendCommand(PlayerApi.addPlayAlbum(album.getId()));
	}

	private void enqueueAlbum(Album album) {
		playerPresenter.rpcSendCommand(PlayerApi.addEnqueueAlbum(album.getId()));
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asWidget());
	}

	public Album getAlbum() {
		return album;
	}

	public AlbumItemPresenter.Display getDisplay() {
		return display;
	}
}
