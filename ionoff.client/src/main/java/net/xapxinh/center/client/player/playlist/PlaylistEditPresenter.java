package net.xapxinh.center.client.player.playlist;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.TextArea;
import com.google.gwt.user.client.ui.TextBox;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.event.ShowLoadingEvent;
import net.xapxinh.center.client.player.event.ShowPlayerMessageEvent;
import net.xapxinh.center.client.player.locale.PlayLocale;
import net.xapxinh.center.shared.dto.PlayListDto;
import net.xapxinh.center.shared.dto.PlayNodeDto;
import net.xapxinh.center.shared.dto.PlayerApi;

public class PlaylistEditPresenter extends AbstractPresenter {

	public interface Display {
		HTMLPanel asPanel();
		
		Label getLblName();
		
		Button getBtnSave();
		
		Button getBtnSaveAs();
		
		Button getBtnClose();

		Button getBtnPlay();

		Button getBtnDelete();
		
		TextBox getTextBoxName();
		
		TextArea getTextAreaDescription();
		
		FlowPanel getNodesWrappers();
	}
	private final Display display;
	private PlayListDto playlist;
	private final PlayerPresenter playerPresenter;

	public PlaylistEditPresenter(HandlerManager eventBus, Display view, PlayerPresenter playerPresenter) {
		super(eventBus);
		display = view;
		this.playerPresenter = playerPresenter;
	}

	public void bind() {
		display.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playPlayList();
			}
		});
		
		display.getBtnSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				savePlaylist();
			}
		});
		
		display.getBtnClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				display.asPanel().removeStyleName("expanded");
				display.asPanel().addStyleName("collapsed");
			}
		});
	}

	private void rpcLoadNodes() {
		if (playerPresenter.getPlayerId() == null) {
			return;
		}
		playerPresenter.getPlayerService().getPlayNodes(playlist.getId(), 
				playerPresenter.getPlayerId(), new MethodCallback<List<PlayNodeDto>>() {
			
			@Override
			public void onFailure(Method method, Throwable exception) {
				playerPresenter.handleException(method, exception);
			}

			@Override
			public void onSuccess(Method method, List<PlayNodeDto> response) {
				eventBus.fireEvent(new ShowLoadingEvent(false));
				playlist.setNodes(response);
				
				display.getNodesWrappers().clear();
				
				for (final PlayNodeDto node : playlist.getNodes()) {
					final PlayNodeView nodeView = new PlayNodeView(node);
					PlayNodePresenter nodePresenter = new PlayNodePresenter(eventBus, node, nodeView, playerPresenter, false);
					nodePresenter.go();
					display.getNodesWrappers().add(nodeView);
					if (node.hasManyLeaf()) {
						nodeView.getBtnRemove().addClickHandler(new ClickHandler() {
							@Override
							public void onClick(ClickEvent event) {
								playlist.getNodes().remove(node);
								display.getNodesWrappers().remove(nodeView);
							}
						});
					}
				}
			}
		});
	}
	
	private void savePlaylist() {
		playlist.setName(display.getTextBoxName().getText());
		playerPresenter.getPlayerService().updatePlayList(playlist.getId(), playlist, playerPresenter.getPlayerId(), 
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

	private void playPlayList() {
		playerPresenter.rpcSendCommand(PlayerApi.addPlayPlaylist(playlist.getId()));
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		display.asPanel().removeStyleName("collapsed");
		display.asPanel().addStyleName("expanded");
	}

	public PlayListDto getPlaylist() {
		return playlist;
	}

	public PlaylistEditPresenter.Display getDisplay() {
		return display;
	}

	public void setPlaylist(PlayListDto playlist) {
		this.playlist = playlist;
		display.getLblName().setText(playlist.getName());
		display.getTextBoxName().setText(playlist.getName());
		rpcLoadNodes();
	}
}
