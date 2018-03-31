package net.xapxinh.center.client.player.playlist;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.locale.PlayLocale;
import net.xapxinh.center.shared.dto.PlayListDto;

public class PlaylistView extends FlowPanel implements PlaylistPresenter.Display {
	
	private final FlowPanel wrapper;
	private final Image imageThumnail;
	private final Button btnPlay;
	private final Button btnExplore;
	private final Button btnDelete;
	
	public PlaylistView(PlayListDto playlist) {

		setStyleName("album playlist col l3 m4 s6");
		
		wrapper = new FlowPanel();
		wrapper.setStyleName("wrapper row");
		add(wrapper);

		final FlowPanel imgWrapper = new FlowPanel();
		imgWrapper.setStyleName("imgWrapper");
		wrapper.add(imgWrapper);
		
		
		imageThumnail = new Image();
		if (playlist.getThumbnail() == null) {
			imageThumnail.setUrl("styles/images/play/playlist.jpg");
		}
		else {
			imageThumnail.setUrl(playlist.getThumbnail());
		}
		imgWrapper.add(imageThumnail);
		imageThumnail.setStyleName("thumnail");

		final FlowPanel btnsWrapper = new FlowPanel();
		btnsWrapper.setStyleName("btns");
		imgWrapper.add(btnsWrapper);

		btnPlay = new Button("");
		btnPlay.addStyleName("play");
		btnsWrapper.add(btnPlay);

		btnExplore = new Button("");
		btnExplore.addStyleName("tracklist");
		btnExplore.setText("");
		btnsWrapper.add(btnExplore);
		
		btnDelete = new Button("");
		btnDelete.addStyleName("delete");
		btnDelete.setText("");
		btnsWrapper.add(btnDelete);

		final FlowPanel panelInfo = new FlowPanel();
		panelInfo.setStyleName("info");
		wrapper.add(panelInfo);

		final InlineLabel lblTitle = new InlineLabel(playlist.getName());
		panelInfo.add(lblTitle);
		lblTitle.setStyleName("title");

		final InlineLabel lblListenCount = new InlineLabel(PlayLocale.getPlayConsts().listenCount() + ": " + getListenCount(playlist));
		panelInfo.add(lblListenCount);
		lblListenCount.setStyleName("detail");
	}

	private String getListenCount(PlayListDto playlist) {
		if (playlist.getListenCount() == null) {
			return "0";
		}
		return playlist.getListenCount().toString();
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
	public Button getBtnExplore() {
		return btnExplore;
	}
	
	@Override
	public Button getBtnDelete() {
		return btnDelete;
	}
}
