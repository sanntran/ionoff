package net.xapxinh.center.client.player.event;

import com.google.gwt.event.shared.GwtEvent;

/**
 * @author Sann Tran
 */
public class MenuItemSelectedEvent extends GwtEvent<MenuItemSelectedEventHandler> {
	
	public static final String ALBUM = "album";
	public static final String BROWSE = "browse";
	public static final String YOUTUBE = "youtube";
	public static final String PLAYLIST = "playlist";
	
	
	public static Type<MenuItemSelectedEventHandler> TYPE = new Type<MenuItemSelectedEventHandler>();
	
	public final String menuItem;
	
	public MenuItemSelectedEvent(String menuItem) {
		this.menuItem = menuItem;
	}
	
	public String getMenuItem() {
		return menuItem;
	}
	
	@Override
	public Type<MenuItemSelectedEventHandler> getAssociatedType() {
		return TYPE;
	}

	@Override
	protected void dispatch(MenuItemSelectedEventHandler handler) {
		handler.onMenuItemSelected(this);
	}
}
