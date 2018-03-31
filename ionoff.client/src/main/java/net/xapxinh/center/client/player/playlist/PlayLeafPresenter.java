package net.xapxinh.center.client.player.playlist;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.shared.dto.PlayLeafDto;
import net.xapxinh.center.shared.dto.PlayerApi;

/**
 * @author Sann Tran
 */
public class PlayLeafPresenter extends AbstractPresenter {

	public interface Display {	
		
		FlowPanel asPanel();
		
		Button getBtnPlay();
		
		Label getLblName();

		Button getBtnRemove();
		
		Label getLblIcon();

		Button getBtnEnqueue();
	}

	private final Display display;
	private PlayLeafDto playLeaf;
	private final boolean isInPlayingList;
	private final PlayerPresenter playerPresenter;

	public PlayLeafPresenter(HandlerManager eventBus, PlayLeafDto playLeaf, Display view, 
			PlayerPresenter playerPresenter, boolean isInPlayingList) {
		
		super(eventBus);
		
		this.display = view;
		this.playLeaf = playLeaf;
		this.isInPlayingList = isInPlayingList;
		this.playerPresenter = playerPresenter;
	}

	public void bind() {
		if (isInPlayingList) {
			display.getBtnPlay().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					playerPresenter.rpcSendCommand(
							PlayerApi.playPlaylistLeaf(playLeaf.getIdx()));
				}
			});
		}
		else {
			display.getBtnPlay().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					playerPresenter.rpcSendCommand(
							PlayerApi.addPlayPlayLeaf(playLeaf.getId()));
				}
			});
			display.getBtnEnqueue().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					playerPresenter.rpcSendCommand(
							PlayerApi.addEnqueuePlayLeaf(playLeaf.getId()));
				}
			});
		}
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
	}

	public Widget getDisplay() {
		return display.asPanel();
	}

	public long getIdx() {
		return playLeaf.getIdx();
	}

	public void updateStyle() {
		if (!playLeaf.isCurrent()) {
			display.asPanel().setStyleName("leaf");
		}
		else {
			display.asPanel().setStyleName("leaf current");
		}
	}

	public void setPlaylistLeaf(PlayLeafDto leaf) {
		playLeaf = leaf; 
		updateStyle();
	}
}
