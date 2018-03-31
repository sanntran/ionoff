package net.xapxinh.center.client.player.youtube;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Image;
import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

public class YoutubeVideoView extends FlowPanel implements YoutubeVideoPresenter.Display {

	private final Button btnPlay;	
	private final Button btnEnqueue;
	private final FlowPanel wrapper;
	
	public YoutubeVideoView(String thumnail, String title, String author, String uploadTime, String duration,
			String viewCount) {

		setStyleName("video col l3 l3 m4 s6");
		
		wrapper = new FlowPanel();
		wrapper.setStyleName("wrapper");
		add(wrapper);
		
		FlowPanel imgWrapper = new FlowPanel();
		imgWrapper.setStyleName("imgWrapper");
		wrapper.add(imgWrapper);
		
		Image imageThumnail = new Image(thumnail);
		imageThumnail.setStyleName("thumnail");
		imgWrapper.add(imageThumnail);

		FlowPanel btnsWrapper = new FlowPanel();
		btnsWrapper.setStyleName("btns");
		imgWrapper.add(btnsWrapper);
		
		Label lblDuration = new InlineLabel(duration);		
		imgWrapper.add(lblDuration);
		lblDuration.setStyleName("duration");
		
		btnPlay = new Button("");
		btnPlay.addStyleName("play");
		btnsWrapper.add(btnPlay);
		
		btnEnqueue = new Button("");
		btnEnqueue.addStyleName("enqueue");
		btnEnqueue.setText("");
		btnsWrapper.add(btnEnqueue);
		
		FlowPanel panelInfo = new FlowPanel();
		panelInfo.setStyleName("info");
		wrapper.add(panelInfo);
		
		Label lblTitle = new Label(title);
		panelInfo.add(lblTitle);
		lblTitle.setStyleName("title");

		InlineLabel lblAuthor = new InlineLabel(author);
		//panelInfo.add(lblAuthor);
		lblAuthor.setStyleName("detail");
		
		Label lblUploadTime = new InlineLabel(uploadTime);
		panelInfo.add(lblUploadTime);
		lblUploadTime.setStyleName("detail date");

		InlineLabel lblViewCount = new InlineLabel(viewCount + " views");
		panelInfo.add(lblViewCount);
		lblViewCount.setStyleName("detail views");
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
}