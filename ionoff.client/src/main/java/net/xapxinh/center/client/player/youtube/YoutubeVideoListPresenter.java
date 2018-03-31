package net.xapxinh.center.client.player.youtube;

import java.util.List;

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
import net.xapxinh.center.client.player.PlayerUtil;
import net.xapxinh.center.shared.dto.YoutubeVideo;
import net.xapxinh.center.shared.dto.YoutubeVideos;

public class YoutubeVideoListPresenter extends AbstractPresenter {

	public interface Display {

		FlowPanel asWidget();

		FlowPanel getPanelVideos();

		TextBox getTextBoxKey();

		Button getBtnSearch();
		
		Button getBtnShowMore();

		YoutubeVideoPresenter.Display newYoutubeVideoView(String thumnail, String title, String author, String uploadTime,
				String duration, String viewCount);
	}
	
	private final HandlerManager eventBus;
	private final PlayerPresenter playerPresenter;
	private YoutubeVideos youtubeVideos;
	private int pageNumber;
	
	protected final Display display;
	
	public YoutubeVideoListPresenter(HandlerManager eventBus, Display view, 
			PlayerPresenter playerPresenter) {

		super(eventBus);
		this.eventBus = eventBus;  
		this.display = view;
		this.playerPresenter = playerPresenter;
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

		display.getBtnSearch().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				search();
			}
		});
		
		display.getBtnShowMore().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				loadMore();
			}
		});
		
		setPageNumber(1);
	}

	private void search() {
		String key = display.getTextBoxKey().getText();
		playerPresenter.searchYoutubeVideo(key, "null");
		setPageNumber(1);
	}

	protected void setPageNumber(int page) {
		pageNumber = page;
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

	private void loadMore() {
		setPageNumber(pageNumber + 1);
		if (youtubeVideos == null || youtubeVideos.getNextPageToken() == null) {
			return;
		}
		String key = display.getTextBoxKey().getText();
		playerPresenter.searchYoutubeVideo(key, youtubeVideos.getNextPageToken());
	}
	
	public void showYoutubeVideos(YoutubeVideos youTubeVideos) {
		youtubeVideos = youTubeVideos;
		
		if (pageNumber == 1) {
			display.getPanelVideos().clear();
		}
		
		if (youtubeVideos.getVideos() == null || youtubeVideos.getVideos().isEmpty()) {
			return;
		}
		int n = youtubeVideos.getVideos().size();
		for (int i = 0; i < n; i++) {
			addYoutubeVideo(youtubeVideos.getVideos(), i);
		}
		display.getBtnShowMore().setVisible(true);
		display.getPanelVideos().add(display.getBtnShowMore());
	}
	
	protected void addYoutubeVideo(List<YoutubeVideo> videos, int index) {
		YoutubeVideoPresenter videoPresenter = createYoutubeVideoInstance(videos.get(index));
		display.getPanelVideos().add(videoPresenter.getDisplay().asWidget());
	}

	protected YoutubeVideoPresenter createYoutubeVideoInstance(final YoutubeVideo youtubeVideo) {
		String thumnail = "";
		if (youtubeVideo.getThumbnail() != null) {
			thumnail = youtubeVideo.getThumbnail();
		}
		String title = youtubeVideo.getTitle();
		if (title.length() > 70) {
			title = title.substring(0, 67) + "...";
		}

		YoutubeVideoPresenter.Display view = display.newYoutubeVideoView(thumnail, title, youtubeVideo.getAuthor(),
				youtubeVideo.getUploadedTime(), PlayerUtil.formatTimePlayer(youtubeVideo.getDuration()),
				youtubeVideo.getViewCount() + "");

		final YoutubeVideoPresenter videoPresenter = new YoutubeVideoPresenter(eventBus,
				youtubeVideo, view, playerPresenter);
		videoPresenter.go();
		return videoPresenter;
	}

	public void reset() {
		pageNumber = 1;
		display.getTextBoxKey().setText("");
	}
}
