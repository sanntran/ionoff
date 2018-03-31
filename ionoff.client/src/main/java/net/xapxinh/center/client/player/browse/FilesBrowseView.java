package net.xapxinh.center.client.player.browse;

import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;
import com.google.gwt.user.client.ui.Widget;

import net.xapxinh.center.client.player.locale.PlayLocale;

public class FilesBrowseView extends FlowPanel implements IFilesBrowseView {
	
	private final Button btnTitle;
	private final Label lblPath;
	private final Button btnBack;
	private final Button btnRefresh;
	private final FlowPanel browseLeafsContainer;
	
	public FilesBrowseView() {
		setStyleName("browse");
		
		FlowPanel header = new FlowPanel();
		header.setStyleName("header");
		add(header);
				
		btnTitle = new Button(PlayLocale.getPlayConsts().file());
		btnTitle.addStyleName("title");
		header.add(btnTitle);
		
		btnBack = new Button("");
		btnBack.addStyleName("back");
		header.add(btnBack);

		btnRefresh = new Button("");
		btnRefresh.addStyleName("refresh");
		header.add(btnRefresh);
		
		lblPath = new Label("");
		lblPath.setStyleName("path");
		header.add(lblPath);
		
		browseLeafsContainer = new FlowPanel();
		browseLeafsContainer.addStyleName("fileList row");
		add(browseLeafsContainer);
	}

	@Override
	public Widget asWidget() {
		return this;
	}
	
	@Override
	public Label getLblPath() {
		return this.lblPath;
	}
	
	@Override
	public Button getBtnTitle() {
		return this.btnTitle;
	}

	@Override
	public HasWidgets getLeafsContainer() {
		return this.browseLeafsContainer;
	}

	@Override
	public Button getBtnBack() {
		return this.btnBack;
	}

	@Override
	public Button getBtnRefresh() {
		return this.btnRefresh;
	}

	@Override
	public MediaFilePresenter.Display newBrowserLeafView() {
		return new MediaFileView();
	}
}
