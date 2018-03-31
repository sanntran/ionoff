package net.xapxinh.center.client.player.playlist;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayerApi;

public class PlaylistPresenter extends AbstractPresenter {

	public interface Display {
		Widget asWidget();
		Button getBtnPlay();
		Button getBtnExplore();
		Button getBtnDelete();
	}
	private final Display display;
	private final PlayListDto playlist;
	private final PlayerPresenter playerPresenter;

	public PlaylistPresenter(HandlerManager eventBus, PlayListDto playlist, Display view,
			PlayerPresenter playerPresenter) {
		super(eventBus);
		display = view;
		this.playlist = playlist;
		this.playerPresenter = playerPresenter;
	}

	public void bind() {
		display.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playPlayList(getPlaylist());
			}
		});
	}

	private void playPlayList(PlayListDto playlist) {
		playerPresenter.rpcSendCommand(PlayerApi.addPlayPlaylist(playlist.getId()));
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

	public PlayListDto getPlaylist() {
		return playlist;
	}

	public PlaylistPresenter.Display getDisplay() {
		return display;
	}
}
