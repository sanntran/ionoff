package net.ionoff.center.client.mediaplayer.album;

import java.util.List;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.user.client.ui.*;

import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.client.mediaplayer.locale.PlayLocale;
import net.ionoff.center.shared.dto.player.Album;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.SongDto;

public class AlbumItemView extends FlowPanel implements AlbumItemPresenter.Display {
	
	private final FlowPanel wrapper;
	private final Image imageThumnail;
	private final Button btnPlay;
	private final Button btnEnqueue;
	private final Button btnTrackList;
	private FlowPanel panelTrackList;

	public AlbumItemView(Album album) {

		setStyleName("album col l3 m4 s6");
		
		wrapper = new FlowPanel();
		wrapper.setStyleName("wrapper row");
		add(wrapper);

		final FlowPanel imgWrapper = new FlowPanel();
		imgWrapper.setStyleName("imgWrapper");
		wrapper.add(imgWrapper);

		imageThumnail = new Image(album.getImage());
		imgWrapper.add(imageThumnail);
		imageThumnail.setStyleName("thumnail");

		final FlowPanel btnsWrapper = new FlowPanel();
		btnsWrapper.setStyleName("btns");
		imgWrapper.add(btnsWrapper);

		btnPlay = new Button("");
		btnPlay.addStyleName("play");
		btnsWrapper.add(btnPlay);

		btnEnqueue = new Button("");
		btnEnqueue.addStyleName("enqueue");
		btnEnqueue.setText("");
		btnsWrapper.add(btnEnqueue);

		btnTrackList = new Button("");
		btnTrackList.addStyleName("tracklist");
		btnTrackList.setText("");
		btnsWrapper.add(btnTrackList);

		final FlowPanel panelInfo = new FlowPanel();
		panelInfo.setStyleName("info");
		wrapper.add(panelInfo);

		final InlineLabel lblTitle = new InlineLabel(album.getTitle());
		panelInfo.add(lblTitle);
		lblTitle.setStyleName("title");

		if (!isNull(album.getAuthor())) {
			final InlineLabel lblAuthor = new InlineLabel(PlayLocale.getPlayConsts().author() + ": " + album.getAuthor());
			panelInfo.add(lblAuthor);
			lblAuthor.setStyleName("detail");
		}
		if (!isNull(album.getArtist())) {
			final InlineLabel lblArtist = new InlineLabel(PlayLocale.getPlayConsts().singer() + ": " + album.getArtist());
			panelInfo.add(lblArtist);
			lblArtist.setStyleName("detail");
		}
		if (!isNull(album.getReleaseDate())) {
			final InlineLabel lblReleaseDate = new InlineLabel(PlayLocale.getPlayConsts().releaseDate() + ": " + album.getReleaseDate());
			panelInfo.add(lblReleaseDate);
			lblReleaseDate.setStyleName("detail");
		}
		final InlineLabel lblListenCount = new InlineLabel(PlayLocale.getPlayConsts().listenCount() + ": " + getListenCount(album));
		panelInfo.add(lblListenCount);
		lblListenCount.setStyleName("detail");
	}

	private String getListenCount(Album album) {
		if (album.getListenCount() == null) {
			return "0";
		}
		return album.getListenCount().toString();
	}

	private boolean isNull(String string) {
		return string == null || "null".equals(string) || "?".equals(string);
	}

	private void addPanelTrackList(List<SongDto> songs, PlayerPresenter playerPresenter) {
		if (songs == null || songs.isEmpty()) {
			return;
		}
		final int songsSize = songs.size();
		for (int i = 0; i < songsSize; i++) {
			SongDto song = songs.get(i);
			final FocusPanel panelTrack = createPanelTrack(song, i + 1);
			panelTrackList.add(panelTrack);
			panelTrack.addClickHandler(clickEvent -> playerPresenter.rpcSendCommand(
					PlayerApi.downloadAlbumSong(song.getId())));
		}
		panelTrackList.setVisible(true);
	}

	private FocusPanel createPanelTrack(SongDto song, int index) {
		FocusPanel focusPanel = new FocusPanel();
		final FlowPanel panelTrack = new FlowPanel();
		panelTrack.setStyleName("track");

		final InlineLabel lblIndex = new InlineLabel(formatIndex(index));
		lblIndex.setStyleName("index");
		panelTrack.add(lblIndex);

		final InlineLabel lblTitle = new InlineLabel(song.getTitle());
		lblTitle.setStyleName("title");
		panelTrack.add(lblTitle);

		final InlineLabel lblArtist = new InlineLabel(song.getArtists());
		lblArtist.setStyleName("artists");
		panelTrack.add(lblArtist);

		focusPanel.setWidget(panelTrack);

		return focusPanel;
	}

	private String formatIndex(int index) {
		if (index < 10) {
			return "0" + index;
		}
		return "" + index;
	}

	@Override
	public Widget asWidget() {
		return this;
	}

	@Override
	public Button getBtnPlay() {
		return btnPlay;
	}

	@Override
	public Button getBtnEnqueue() {
		return btnEnqueue;
	}

	@Override
	public Button getBtnTrackList() {
		return btnTrackList;
	}

	@Override
	public void showOrHidePanelTracks(List<SongDto> songs, PlayerPresenter playerPresenter) {
		if (panelTrackList == null) {
			panelTrackList = new FlowPanel();
			panelTrackList.setStyleName("tracks");
			panelTrackList.setVisible(false);
			addPanelTrackList(songs, playerPresenter);
			wrapper.add(panelTrackList);
			return;
		}
		if (panelTrackList.isVisible()) {
			panelTrackList.setVisible(false);
		}
		else {
			panelTrackList.setVisible(true);
		}
	}
}
