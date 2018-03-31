package net.xapxinh.center.client.player.browse;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.browse.MediaFilePresenter.Display;

public interface IFilesBrowseView {
	Widget asWidget();
	Button getBtnTitle();
	Button getBtnBack();
	Button getBtnRefresh();
	HasWidgets getLeafsContainer();
	MediaFilePresenter.Display newBrowserLeafView();
	Label getLblPath();
}
