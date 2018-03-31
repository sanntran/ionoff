package net.xapxinh.center.client.player.playing;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.TextBox;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.client.player.event.ShowPlayerMessageEvent;
import net.xapxinh.center.client.player.locale.PlayLocale;
import net.xapxinh.center.client.player.playlist.PlayNodePresenter;
import net.xapxinh.center.client.player.playlist.PlayNodeView;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;
import net.xapxinh.center.shared.dto.PlayerApi;
import net.xapxinh.center.shared.dto.Status;

/**
 * @author Sann Tran
 */
public class PlayingListPresenter extends AbstractPresenter {

	public interface Display {

		Widget asWidget();
		
		FlowPanel getNodeContainer();
		
		Button getBtnIcon();

		Label getLblTitle();
		
		Button getBtnSave();
		
		Button getBtnSaveAs();
		
		Button getBtnPlayMode();

		Button getBtnEmpty();
		
		Button getBtnRandom();
		
		Button getBtnRepeat();
		
		Button getBtnLoop();
		
		PopupPanel getPopupPlayModes();

		Button getBtnOkSave();

		TextBox getTextBoxName();

		PopupPanel getPopupSavePlaylist();
		
		FlowPanel getPopupSavePlaylistWrapper();

	}
	
	private static final String SAVE = "Save";
	private static final String SAVE_AS = "SaveAs";

	protected final Display display;
	protected final PlayerPresenter playerPresenter;
	private List<PlayNodePresenter> nodePresenters;
	private String saveMode = SAVE;
	private PlayListDto playingList;
	
	public PlayingListPresenter(HandlerManager eventBus, Display view, PlayerPresenter playerPresenter) {
		super(eventBus);
		this.display = view;
		this.playerPresenter = playerPresenter;
		nodePresenters = new ArrayList<PlayNodePresenter>();
	}

	public void bind() {
		display.getBtnLoop().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playerPresenter.rpcSendCommand(PlayerApi.loopPlaylist(), false);
			}
		});
		display.getBtnPlayMode().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (display.getPopupPlayModes().isAttached()) {
					display.getPopupPlayModes().hide();
				}
				else {
					Widget source = (Widget) event.getSource();
					int left = source.getAbsoluteLeft();
			        int top = source.getAbsoluteTop() + 45;
			        display.getPopupPlayModes().setPopupPosition(left, top);
			        display.getPopupPlayModes().show();
				}
				
			}
		});
		display.getBtnRepeat().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playerPresenter.rpcSendCommand(PlayerApi.repeatPlaylistLeaf(), false);
			}
		});
		display.getBtnEmpty().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playerPresenter.rpcSendCommand(PlayerApi.emptyPlaylist());
			}
		});
		display.getBtnRandom().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playerPresenter.rpcSendCommand(PlayerApi.randomPlaylist(), false);
			}
		});
		display.getBtnSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (playingList == null) {
					return;
				}
				if (playingList.isNew()) {
					saveMode = SAVE_AS;
				}
				else {
					saveMode = SAVE;
				}
				display.getTextBoxName().setText(playingList.getName());
				Widget source = (Widget) event.getSource();
				int left = display.asWidget().getAbsoluteLeft();
		        int top = source.getAbsoluteTop() + 45;
		        display.getPopupSavePlaylist().setWidth(display.asWidget().getOffsetWidth() + "px");
		        display.getPopupSavePlaylistWrapper().setWidth(display.asWidget().getOffsetWidth() + "px");
		        display.getPopupSavePlaylist().setPopupPosition(left, top);
				display.getPopupSavePlaylist().show();
			}
		});
		
		display.getBtnSaveAs().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (playingList == null) {
					return;
				}
				saveMode = SAVE_AS;
				display.getTextBoxName().setText(playingList.getName());
				Widget source = (Widget) event.getSource();
				int left = display.asWidget().getAbsoluteLeft();
		        int top = source.getAbsoluteTop() + 45;
		        display.getPopupSavePlaylist().setWidth(display.asWidget().getOffsetWidth() + "px");
		        display.getPopupSavePlaylistWrapper().setWidth(display.asWidget().getOffsetWidth() + "px");
		        display.getPopupSavePlaylist().setPopupPosition(left, top);
				display.getPopupSavePlaylist().show();
			}
		});
		display.getBtnOkSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (SAVE_AS.equals(saveMode)) {
					rpcSavePlaylistAs();
				}
				else {
					rpcSavePlaylist();
				}
			}
		});
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
	
	private void rpcSavePlaylist() {
		playingList.setName(display.getTextBoxName().getText());
		playerPresenter.getPlayerService().updatePlayList(playingList.getId(), playingList, playerPresenter.getPlayerId(), 
				new MethodCallback<PlayListDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, PlayListDto response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				eventBus.fireEvent(new ShowPlayerMessageEvent(PlayLocale.getPlayMessages().playlistSaved(), 
						ShowPlayerMessageEvent.SUCCESS));
			}
		});
	}

	private void rpcSavePlaylistAs() {
		playingList.setName(display.getTextBoxName().getText());
		playerPresenter.getPlayerService().insertPlayList(playingList, playerPresenter.getPlayerId(),  
				new MethodCallback<PlayListDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}
			@Override
			public void onSuccess(Method method, PlayListDto response) {
				eventBus.fireEvent(new ShowPlayerMessageEvent(PlayLocale.getPlayMessages().playlistSaved(), 
						ShowPlayerMessageEvent.SUCCESS));
			}
		});
	}
	
	protected void showPlaylist(PlayListDto playlist) {
		
		playingList = playlist;
		display.getNodeContainer().clear();
		if (playlist == null) {
			return;
		}
		
		if (playlist.getName() == null) {
			display.getLblTitle().setText(PlayLocale.getPlayConsts().playing());
		} else {
			display.getLblTitle().setText(PlayLocale.getPlayConsts().playing() + " (" + playlist.getName() + ")");
		}
		if (playlist.getNodes().isEmpty()) {
			return;
		}
		List<PlayNodePresenter> newNodePresenters = new ArrayList<PlayNodePresenter>();
		for (PlayNodeDto node : playlist.getNodes()) {	
			PlayNodePresenter nodePresenter = findNodePresenter(node);
			if (nodePresenter != null) {
				nodePresenter.setPlaylistNode(node);
				newNodePresenters.add(nodePresenter);
				display.getNodeContainer().add(nodePresenter.getDisplay());
			}
			else {
				PlayNodeView nodeView = new PlayNodeView(node);
				if (nodeView.getBtnEnqueue() != null) {
					nodeView.getBtnEnqueue().setVisible(false);
				}
				nodePresenter = new PlayNodePresenter(eventBus, node, nodeView, playerPresenter, true);
				newNodePresenters.add(nodePresenter);
				nodePresenter.go();
				display.getNodeContainer().add(nodeView);
			}
		}
		nodePresenters = newNodePresenters;
	}

	private PlayNodePresenter findNodePresenter(PlayNodeDto node) {
		for (PlayNodePresenter nodePresenter : nodePresenters) {
			if (nodePresenter.getIdx() == node.getIdx() && node.hasLeaf() 
					&& node.getName() != null
					&& node.getName().equals(nodePresenter.getNode().getName())) {
				return nodePresenter;
			}
		}
		return null;
	}

	public void setStatus(Status status) {
		if (status != null) {
			if (status.getLoop() == false) {
				display.getBtnLoop().setStyleName("gwt-Button mode loop");
			}
			else {
				display.getBtnLoop().setStyleName("gwt-Button mode loop activated");
			}
			if (status.getRepeat() == false) {
				display.getBtnRepeat().setStyleName("gwt-Button mode repeat");
			}
			else {
				display.getBtnRepeat().setStyleName("gwt-Button mode repeat activated");
			}
			if (status.getRandom() == false) {
				display.getBtnRandom().setStyleName("gwt-Button mode random");
			}
			else {
				display.getBtnRandom().setStyleName("gwt-Button mode random activated");
			}
		}
	}
}
