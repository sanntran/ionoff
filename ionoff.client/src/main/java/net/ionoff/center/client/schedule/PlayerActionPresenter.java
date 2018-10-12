package net.ionoff.center.client.schedule;

import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ChangeEvent;
import com.google.gwt.event.dom.client.ChangeHandler;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SchedulePlayerActionDto;
import net.ionoff.center.client.mediaplayer.event.RpcFailureEvent;
import net.ionoff.center.shared.dto.player.MediaFile;
import net.ionoff.center.shared.dto.player.PlayerApi;

public abstract class PlayerActionPresenter<T extends BaseDto> extends SceneActionPresenter {
	
	protected final IRpcServiceProvider rpcProvider;
	protected final IPlayerActionView display;
	protected String parentPath;
	protected String currentPath;
	
	public PlayerActionPresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
			IPlayerActionView view) {
		super(eventBus);
		this.rpcProvider = rpcProvider;
		this.display = view;
	}
	
	public void bind() {
		display.getListBoxActions().addValueChangeHandler(new ValueChangeHandler<String>() {
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				String action = getSelectedAction();
				onUpdateAction(action);
			}
		});
		
		display.getIntBoxVolume().addChangeHandler(new ChangeHandler() {
			@Override
			public void onChange(ChangeEvent event) {
				setTargetVolume(display.getIntBoxVolume().getText());
			}
		});
		
		display.getTextBoxAlbum().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				String folder = MediaFile.getParent(getTargetAlbum());
				rpcGetMediaFiles(folder);
				display.getAlbumContainer().setVisible(true);
			}
		});
	}
	
	private String getSelectedAction() {
		String action = SchedulePlayerActionDto.NONE;
		if (display.getListBoxActions().getSelectedIndex() == 1) {
			action = SchedulePlayerActionDto.PLAY;
		}
		else if (display.getListBoxActions().getSelectedIndex() == 2) {
			action = SchedulePlayerActionDto.STOP;
		}
		return action;
	}
	
	private void onUpdateAction(String action) {
		setTargetAction(action);
		if (SchedulePlayerActionDto.PLAY.equals(action)) {
			display.getAlbumSelectionPanel().setVisible(true);
		}
		else {
			display.getAlbumSelectionPanel().setVisible(false);
		}
	}
	
	protected abstract Long getTargetId();
	protected abstract String getTargetAction();
	protected abstract String getTargetVolume();
	protected abstract String getTargetAlbum();
	protected abstract String getTargetAlbumType();
	protected abstract Long getPlayerId();
	protected abstract String getPlayerName();
	protected abstract String getTargetClazz();

	protected abstract void setTarget(T target);
	protected abstract void setTargetAction(String action);
	protected abstract void setTargetVolume(String volume);
	protected abstract void setTargetAlbum(String album);
	protected abstract void setTargetAlbumType(String albumType);

	public void rpcGetMediaFiles() {
		rpcGetMediaFiles(currentPath);
	}
	
	public void rpcGetMediaFiles(String path) {
		if (path == null) {
			path = "";
		}
		rpcProvider.getPlayerService().getMediaFiles(getPlayerId(), path, new MethodCallback<List<MediaFile>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				eventBus.fireEvent(new RpcFailureEvent(method, exception));
			}
			@Override
			public void onSuccess(Method method, List<MediaFile> response) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				showMediaFiles(response);
			}
		});
	}

	private void showMediaFiles(List<MediaFile> files) {
		display.getAlbumContainer().clear();
		
		for (final MediaFile file : files) {
			
			if (PlayerApi.PARENT.equals(file.getName())) {
				parentPath = file.getPath();
				currentPath = parentPath.replace("\\..", "").replaceAll("/..", "");
				display.getTextBoxAlbum().setText(currentPath);
				display.getTextBoxAlbum().setCursorPos(currentPath.length());
			}
			
			PlayerAlbumView albumView = new PlayerAlbumView(file);
			display.getAlbumContainer().add(albumView);
			
			if (PlayerApi.DIR.equalsIgnoreCase(file.getType())) {
				albumView.getBtnIcon().addClickHandler(new ClickHandler() {
					@Override
					public void onClick(ClickEvent event) {
						setScheduleAlbum(file);
						rpcGetMediaFiles(file.getPath());
					}
				});
			}
			
			albumView.getBtnSelect().addClickHandler(new ClickHandler() {
				@Override
				public void onClick(ClickEvent event) {
					setScheduleAlbum(file);
					display.getAlbumContainer().setVisible(false);
				}
			});
		}
	}
	
	private void setScheduleAlbum(MediaFile file) {
		String name = "";
		String type = "";
		if (file != null) {
			name = file.getPath();
			type = file.getType();
		}
		setScheduleAlbum(name, type);
	}
	
	private void setScheduleAlbum(String name, String type) {
		setTargetAlbum(name);
		setTargetAlbumType(type);
		display.getTextBoxAlbum().setText(name);
		display.getTextBoxAlbum().setCursorPos(name.length());
	}
	
	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.add(display.asPanel());
	}

	protected void updateDisplay() {
		display.getIntBoxVolume().setText(getTargetVolume());
		display.getTextBoxAlbum().setText(getTargetAlbum());

		if (SchedulePlayerActionDto.NONE.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(0);
			display.getAlbumSelectionPanel().setVisible(false);
		}
		else if (SchedulePlayerActionDto.PLAY.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(1);
			display.getAlbumSelectionPanel().setVisible(true);
		}
		else if (SchedulePlayerActionDto.STOP.equals(getTargetAction())) {
			display.getListBoxActions().setSelectedIndex(2);
			display.getAlbumSelectionPanel().setVisible(false);
		}
	}
}
