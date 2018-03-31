package net.xapxinh.center.client.player.menu;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.event.MenuItemSelectedEvent;

/**
 * @author Sann Tran
 */
public class PlayerMenuPresenter extends AbstractPresenter {
	
	public interface Display {
		FlowPanel asPanel();
		MenuItemView getAlbumMenuItem();
		MenuItemView getFileMenuItem();
		MenuItemView getYoutubeMenuItem();
		MenuItemView getPlaylistMenuItem();
		void hide();
		void show();
		boolean isShowed();
	}
	
	private final Display display;
	
	public PlayerMenuPresenter(HandlerManager eventBus, Display view) {
		super(eventBus);
		this.display = view;
	}
	
	@Override
	public void show(HasWidgets container) {
		getDisplay().show();
	}

	@Override
	public void go() {
		bind();
	}
	
	public void bind() {

		display.getAlbumMenuItem().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getDisplay().hide();
				setDefaultMenuItemsStyle();
				eventBus.fireEvent(new MenuItemSelectedEvent(MenuItemSelectedEvent.ALBUM));
			}
		});
		
		//
		display.getFileMenuItem().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getDisplay().hide();
				setDefaultMenuItemsStyle();
				eventBus.fireEvent(new MenuItemSelectedEvent(MenuItemSelectedEvent.BROWSE));
			}
		});
		//
		display.getYoutubeMenuItem().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				getDisplay().hide();
				setDefaultMenuItemsStyle();
				eventBus.fireEvent(new MenuItemSelectedEvent(MenuItemSelectedEvent.YOUTUBE));
			}
		});
		//
		display.getPlaylistMenuItem().addClickHandler(new ClickHandler() {
		@Override
		public void onClick(ClickEvent event) {
			getDisplay().hide();
			setDefaultMenuItemsStyle();
			eventBus.fireEvent(new MenuItemSelectedEvent(MenuItemSelectedEvent.PLAYLIST));
		}
	});
	}
	
	public PlayerMenuPresenter.Display getDisplay() {
		return this.display;
	}
	
	public void setDefaultMenuItemsStyle() {
		display.getFileMenuItem().setStyleName("menuItem");
		display.getAlbumMenuItem().setStyleName("menuItem");
		display.getYoutubeMenuItem().setStyleName("menuItem");	
		display.getPlaylistMenuItem().setStyleName("menuItem");	
	}
}
