package net.xapxinh.center.client.player.browse;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.Label;

public class MediaFileView extends FlowPanel implements MediaFilePresenter.Display {

	private final Button btnIcon;
	private final Label lblName;
	private final Button btnPlay;
	private final Button btnEnqueue;

	public MediaFileView() {
		setStyleName("leaf");

		btnIcon = new Button("");
		add(btnIcon);

		lblName = new Label();
		lblName.setStyleName("name");
		add(lblName);

		btnEnqueue = new Button("");
		btnEnqueue.addStyleName("enqueue");
		add(btnEnqueue);
		
		btnPlay = new Button("");
		btnPlay.addStyleName("play");
		add(btnPlay);
	}

	@Override
	public Button getBtnIcon() {
		return this.btnIcon;
	}

	@Override
	public Label getLblName() {
		return this.lblName;
	}

	@Override
	public FlowPanel asWidget() {
		return this;
	}

	@Override
	public Button getBtnPlay() {
		return this.btnPlay;
	}

	@Override
	public Button getBtnEnqueue() {
		return this.btnEnqueue;
	}
}