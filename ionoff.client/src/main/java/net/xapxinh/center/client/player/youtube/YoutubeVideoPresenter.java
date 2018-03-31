package net.xapxinh.center.client.player.youtube;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.shared.dto.Command;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.YoutubeVideo;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Widget;

public class YoutubeVideoPresenter extends AbstractPresenter {

	public interface Display {
		Widget asWidget();

		Button getBtnPlay();

		Button getBtnEnqueue();
	}

	private final Display display;
	private YoutubeVideo youtubeVideo;
	private final PlayerPresenter playerPresenter;
	
	public YoutubeVideoPresenter(
			HandlerManager eventBus, YoutubeVideo youtubeVideo, Display view, 
			PlayerPresenter playerPresenter) {
		super(eventBus);
		this.display = view;
		this.youtubeVideo = youtubeVideo;
		this.playerPresenter = playerPresenter;
	}

	public void bind() {
		
		display.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playVideo(getYoutubeVideo());
			}
		});
		
		display.getBtnEnqueue().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				enqueueVideo(getYoutubeVideo());
			}
		});
	}
	
	public void playVideo(YoutubeVideo youtubeVideo) {
		Command cmd = PlayerApi.addPlayYoutube(youtubeVideo.getId());
		playerPresenter.rpcSendCommand(cmd.setTitle(youtubeVideo.getTitle()));
	}

	public void enqueueVideo(YoutubeVideo youtubeVideo) {
		Command cmd = PlayerApi.addEnqueueYoutube(youtubeVideo.getId());
		playerPresenter.rpcSendCommand(cmd.setTitle(youtubeVideo.getTitle()));
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

	public YoutubeVideo getYoutubeVideo() {
		return this.youtubeVideo;
	}

	public YoutubeVideoPresenter.Display getDisplay() {
		return display;
	}
}
