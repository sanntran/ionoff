package net.xapxinh.center.client.player.album;

import java.util.List;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.locale.PlayLocale;
import net.xapxinh.center.shared.dto.Album;
import net.xapxinh.center.shared.dto.Song;

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

	private void addPanelTrackList(List<Song> songs) {
		if (songs == null || songs.isEmpty()) {
			return;
		}
		final int songsSize = songs.size();
		for (int i = 0; i < songsSize; i++) {
			final FlowPanel panelTrack = createPanelTrack(songs.get(i), i + 1);
			panelTrackList.add(panelTrack);
		}
		panelTrackList.setVisible(true);
	}

	private FlowPanel createPanelTrack(Song song, int index) {
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

		return panelTrack;
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
	public void showOrHidePanelTracks(List<Song> songs) {
		if (panelTrackList == null) {
			panelTrackList = new FlowPanel();
			panelTrackList.setStyleName("tracks");
			panelTrackList.setVisible(false);
			addPanelTrackList(songs);
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
