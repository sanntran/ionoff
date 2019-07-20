package net.ionoff.center.client.mediaplayer.playlist;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.FocusPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;

import net.ionoff.center.client.mediaplayer.AbstractPresenter;
import net.ionoff.center.client.mediaplayer.PlayerPresenter;
import net.ionoff.center.shared.dto.player.PlayLeafDto;
import net.ionoff.center.shared.dto.player.PlayNodeDto;
import net.ionoff.center.shared.dto.player.PlayerApi;

/**
 * @author Sann Tran
 */
public class PlayNodePresenter extends AbstractPresenter {

	public interface Display {	
		
		FlowPanel asPanel();
		
		FocusPanel getFocusPanel();
		
		Button getBtnPlay();
		
		Label getLblName();

		Button getBtnRemove();
		
		FlowPanel getLeafContainer();

		Button getBtnEnqueue();
	}
	private PlayNodeDto playlistNode;
	private final Display display;	
	private final boolean isInPlayingList;
	private final PlayerPresenter playerPresenter;
	private final List<PlayLeafPresenter> leafPresenters;

	public PlayNodePresenter(HandlerManager eventBus, PlayNodeDto playlistNode, Display view, 
			PlayerPresenter playerPresenter, boolean isInPlayingList) {
		super(eventBus);
		this.display = view;
		this.playlistNode = playlistNode;
		this.playerPresenter = playerPresenter;
		this.isInPlayingList = isInPlayingList;
		leafPresenters = new ArrayList<>();
	}

	public void bind() {
		if (display.getBtnEnqueue() != null && !isInPlayingList) {
			display.getBtnEnqueue().addClickHandler(event -> playerPresenter.rpcSendCommand(
					PlayerApi.addEnqueuePlayNode(playlistNode.getId())));
		}
		
		if (display.getBtnPlay() != null && !isInPlayingList) {
			display.getBtnPlay().addClickHandler(event -> playerPresenter.rpcSendCommand(
					PlayerApi.addPlayPlayNode(playlistNode.getId())));
		}
		
		if (!isInPlayingList) {
			showLeafs();
		}
		if  (playlistNode.hasManyLeaf()) {
			display.getFocusPanel().addClickHandler(event -> {
				if (display.getLeafContainer().isVisible()) {
					display.getLeafContainer().setVisible(false);
				}
				else {
					showLeafs();
				}
			});
			display.getBtnRemove().addClickHandler(event -> playerPresenter.rpcSendCommand(
					PlayerApi.deletePlaylistNode(playlistNode.getIdx())));
		}
		else {
			showLeafs();
		}
		
	}

	private void showLeafs() {
		if (!leafPresenters.isEmpty()) {
			display.getLeafContainer().setVisible(true);
			return;
		}
		if (playlistNode.getLeafs().isEmpty()) {
			return;
		}
		for (final PlayLeafDto leaf : playlistNode.getLeafs()) {
			final PlayLeafView leafView = new PlayLeafView(leaf);
			PlayLeafPresenter leafPresenter = new PlayLeafPresenter(eventBus, leaf, leafView, playerPresenter, isInPlayingList);
			leafPresenters.add(leafPresenter);
			leafPresenter.go();
			
			if (isInPlayingList) {
				leafView.getBtnEnqueue().setVisible(false);
				leafView.getBtnRemove().addClickHandler(event -> playerPresenter.rpcSendCommand(
						PlayerApi.deletePlaylistLeaf(leaf.getIdx())));
				leafView.getLblIcon().addClickHandler(event -> playerPresenter.rpcSendCommand(
						PlayerApi.downloadPlaylistLeaf(leaf.getIdx())));
			}
			else {
				leafView.getBtnRemove().addClickHandler(event -> {
					playlistNode.getLeafs().remove(leaf);
					display.getLeafContainer().remove(leafView);
				});
			}

			display.getLeafContainer().add(leafView);
		}
		display.getLeafContainer().setVisible(true);
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
	}

	public FlowPanel getDisplay() {
		return display.asPanel();
	}

	public long getIdx() {
		return playlistNode.getIdx();
	}
	
	public PlayNodeDto getNode() {
		return playlistNode;
	}

	public void setPlaylistNode(PlayNodeDto node) {
		playlistNode = node;
		for (int i = 0; i < leafPresenters.size(); i++) {
			PlayLeafPresenter leafPresenter = leafPresenters.get(i);
			if (!playlistNode.hasLeaf(leafPresenter.getIdx())) {
				leafPresenters.remove(i);
				display.getLeafContainer().remove(leafPresenter.getDisplay());
				i--;
			}
			else {
				leafPresenter.setPlaylistLeaf(node.findLeaf(leafPresenter.getIdx()));
			}
		}
		updateStyle();
	}

	public void updateStyle() {
		if (!playlistNode.isCurrent()) {
			display.asPanel().setStyleName("node");
		}
		else {
			display.asPanel().setStyleName("node current");
		}
		for (PlayLeafPresenter leafPresenter : leafPresenters) {
			leafPresenter.updateStyle();
		}
	}
}
