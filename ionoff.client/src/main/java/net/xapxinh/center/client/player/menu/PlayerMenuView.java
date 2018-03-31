package net.xapxinh.center.client.player.menu;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.CloseEvent;
import com.google.gwt.event.logical.shared.CloseHandler;
import com.google.gwt.user.client.Timer;
import com.google.gwt.user.client.ui.CheckBox;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.PopupPanel;
import com.google.gwt.user.client.ui.PopupPanel.AnimationType;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class PlayerMenuView extends FlowPanel implements PlayerMenuPresenter.Display {
	
	private final CheckBox toggle;
	private final Timer toggleTimer; 
	private final MenuItemView albumMenuItem;
	private final MenuItemView fileMenuItem;
	private final MenuItemView youtubeMenuItem;
	private final MenuItemView playlistMenuItem;
	
	private PopupPanel popupMenu;
	
	public PlayerMenuView() {		
		setStyleName("menu");
		
		toggle = new CheckBox("");
		toggle.setStyleName("toggle");
		add(toggle);
		
		toggle.addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				if (toggle.getValue()) {
					popupMenu.show();
				}
				else {
					popupMenu.hide();
				}
			}
		});
		toggleTimer = new Timer() {
			@Override
			public void run() {
				toggle.setValue(false);
			}
		};
		
		popupMenu = new PopupPanel(true, true);
		popupMenu.addCloseHandler(new CloseHandler<PopupPanel>() {
			@Override
			public void onClose(CloseEvent<PopupPanel> arg0) {
				toggleTimer.schedule(100);
			}
		});
		popupMenu.setStyleName("popup playerMenu");
		popupMenu.setAnimationEnabled(true);
		popupMenu.setAnimationType(AnimationType.ROLL_DOWN);
		
		FlowPanel wrapper = new FlowPanel();
		wrapper.addStyleName("wrapper");
		popupMenu.setWidget(wrapper);
		
		albumMenuItem = new MenuItemView();
		albumMenuItem.setIconStyleName("icon album");
		albumMenuItem.setText(PlayLocale.getPlayConsts().album());
		wrapper.add(albumMenuItem);
		
		fileMenuItem = new MenuItemView();
		fileMenuItem.setIconStyleName("icon browse");
		fileMenuItem.setText(PlayLocale.getPlayConsts().file());
		wrapper.add(fileMenuItem);
		
		youtubeMenuItem = new MenuItemView();
		youtubeMenuItem.setIconStyleName("icon youtube");
		youtubeMenuItem.setText(PlayLocale.getPlayConsts().youtube());
		wrapper.add(youtubeMenuItem);
		
		playlistMenuItem = new MenuItemView();
		playlistMenuItem.setIconStyleName("icon playlist");
		playlistMenuItem.setText(PlayLocale.getPlayConsts().playlist());
		wrapper.add(playlistMenuItem);
	}
	
	@Override
	public FlowPanel asPanel() {
		return this;
	}
	
	@Override
	public MenuItemView getAlbumMenuItem() {
		return this.albumMenuItem;
	}
	
	@Override
	public MenuItemView getFileMenuItem() {
		return this.fileMenuItem;
	}
	
	@Override
	public MenuItemView getYoutubeMenuItem() {
		return this.youtubeMenuItem;
	}
	
	@Override
	public MenuItemView getPlaylistMenuItem() {
		return this.playlistMenuItem;
	}

	@Override
	public boolean isShowed() {
		return getStyleName().contains("show");
	}
	
	@Override
	public void hide() {
		toggle.setValue(false);
		popupMenu.hide();
	}
	@Override
	public void show() {
		toggle.setValue(true);
		popupMenu.show();
	}
}