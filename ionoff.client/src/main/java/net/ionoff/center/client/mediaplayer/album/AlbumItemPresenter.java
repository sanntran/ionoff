package net.ionoff.center.client.mediaplayer.album;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import net.ionoff.center.client.mediaplayer.AbstractPresenter;
import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.client.mediaplayer.event.ShowLoadingEvent;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.SongDto;

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

		void showOrHidePanelTracks(List<SongDto> songs, PlayerPresenter playerPresenter);
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
		display.getBtnTrackList().addClickHandler(event -> {
			if (album.getSongs() == null || album.getSongs().isEmpty()) {
				rpcLoadSongs();
			}
			else {
				display.showOrHidePanelTracks(album.getSongs(), playerPresenter);
			}
		});
		display.getBtnPlay().addClickHandler(event -> playAlbum(getAlbum()));

		display.getBtnEnqueue().addClickHandler(event -> enqueueAlbum(getAlbum()));
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
				display.showOrHidePanelTracks(album.getSongs(), playerPresenter);

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
