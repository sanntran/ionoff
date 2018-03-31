package net.ionoff.center.client.navigation;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.PopupPanel;

public class PopupProjectsView extends PopupPanel {
	
	private final FlowPanel wrapper;
	private final FlowPanel contentWrapper;
	private final List<ProjectMenuItemView> menuItemViews;
	
	public PopupProjectsView() {
		super(true, true);
		setStyleName("popupProjects");
		setAnimationEnabled(true);
		setAnimationType(AnimationType.ROLL_DOWN);
		
		wrapper = new FlowPanel();
		setWidget(wrapper);
		
		contentWrapper = new FlowPanel();
		contentWrapper.addStyleName("wrapper");
		wrapper.add(contentWrapper);
		
		menuItemViews = new ArrayList<>();
	}
	
	public FlowPanel getContentWrapper() {
		return contentWrapper;
	}

	public List<ProjectMenuItemView> getMenuItemViews() {
		return menuItemViews;
	}
}
