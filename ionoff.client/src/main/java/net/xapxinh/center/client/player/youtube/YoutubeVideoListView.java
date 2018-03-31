package net.xapxinh.center.client.player.youtube;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextBox;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class YoutubeVideoListView extends FlowPanel implements YoutubeVideoListPresenter.Display {
	
	private final Label lblTitle;
	private Button btnSearch;
	private TextBox textBoxKey;
	private Button btnShowMore;
	
	private final FlowPanel panelVideos;
	
	public YoutubeVideoListView() {
		setStyleName("youtube row");
		setSize("100%", "");
		
		FlowPanel panelSearch = new FlowPanel();
		panelSearch.setStyleName("header");
		add(panelSearch);
		
		lblTitle = new Label(PlayLocale.getPlayConsts().youtube());
		panelSearch.add(lblTitle);
		lblTitle.addStyleName("title");
		
		textBoxKey = new TextBox();
		textBoxKey.addStyleName("textbox");
		panelSearch.add(textBoxKey);
		
		btnSearch = new Button("");
		btnSearch.setText("");
		panelSearch.add(btnSearch);
		btnSearch.addStyleName("search");
		
		panelVideos = new FlowPanel();
		panelVideos.setStyleName("videos");
		add(panelVideos);
		
		// Paging
		btnShowMore = new Button(PlayLocale.getPlayConsts().showMore());
		panelVideos.add(btnShowMore);
		btnShowMore.setStyleName("showMore");
		btnShowMore.setVisible(false);
	}

	@Override
	public FlowPanel asWidget() {
		return this;
	}

	@Override
	public FlowPanel getPanelVideos() {
		return this.panelVideos;
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
	public YoutubeVideoPresenter.Display newYoutubeVideoView(String thumnail, String title, String author,
			String uploadTime, String duration, String viewCount) {
		return new YoutubeVideoView(thumnail, title, author, uploadTime, duration, viewCount);
	}

	@Override
	public Button getBtnShowMore() {
		return this.btnShowMore;
	}

}
