package net.ionoff.center.client.mediaplayer.youtube;

import net.ionoff.center.client.mediaplayer.AbstractPresenter;
import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.shared.dto.player.Command;
import net.ionoff.center.shared.dto.player.PlayerApi;
import net.ionoff.center.shared.dto.player.YoutubeVideoDto;

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
	private YoutubeVideoDto youtubeVideo;
	private final PlayerPresenter playerPresenter;
	
	public YoutubeVideoPresenter(
            HandlerManager eventBus, YoutubeVideoDto youtubeVideo, Display view,
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
	
	public void playVideo(YoutubeVideoDto youtubeVideo) {
		Command cmd = PlayerApi.addPlayYoutube(youtubeVideo.getId());
		playerPresenter.rpcSendCommand(cmd.setTitle(youtubeVideo.getTitle()));
	}

	public void enqueueVideo(YoutubeVideoDto youtubeVideo) {
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

	public YoutubeVideoDto getYoutubeVideo() {
		return this.youtubeVideo;
	}

	public YoutubeVideoPresenter.Display getDisplay() {
		return display;
	}
}
