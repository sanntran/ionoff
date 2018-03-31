package net.xapxinh.center.client.player.browse;

import java.util.List;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.client.player.PlayerPresenter;
import net.xapxinh.center.client.player.browse.MediaFilePresenter.Display;
import net.xapxinh.center.shared.dto.MediaFile;
import net.xapxinh.center.shared.dto.PlayerApi;

/**
 * @author Sann Tran
 */
public class FilesBrowsePresenter extends AbstractPresenter {

	protected final IFilesBrowseView display;
	protected final PlayerPresenter playerPresenter;
	
	protected String rootPath;
	protected String parentPath;
	protected String currentPath;

	public FilesBrowsePresenter(HandlerManager eventBus, IFilesBrowseView view, 
			PlayerPresenter playerPresenter) {
		super(eventBus);	
		this.display = view;
		this.playerPresenter = playerPresenter;
	}	
	
	private void bind() {
		display.getBtnTitle().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				requestBrowseMedia();
			}
		});
		
		display.getBtnBack().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				requestBrowseMedia(parentPath);
			}
		});
		display.getBtnRefresh().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				refreshBrowseMedia(currentPath);
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
		if (currentPath == null || currentPath.isEmpty()) {
			requestBrowseMedia();
		}
	}

	protected void openLeaf(MediaFile file) {
		if (PlayerApi.FILE.equals(file.getType())) {
			playLeaf(file);
		}
		else if (PlayerApi.DIR.equals(file.getType())) {
			requestBrowseMedia(file.getPath());
		}
	}

	public void showMediaFiles(List<MediaFile> files) {
		display.getLeafsContainer().clear();
		for (MediaFile file : files) {
			if (PlayerApi.PARENT.equals(file.getName())) {
				parentPath = file.getPath();
				currentPath = parentPath.replace("\\..", "").replaceAll("/\\.\\.", "");
				if (rootPath == null) {
					rootPath = currentPath;
				}
				currentPath = currentPath.replaceAll(rootPath, "");
				display.getLblPath().setText(currentPath);
			}
			else {
				MediaFilePresenter leaf = createBrowserLeafInstance(file);
				display.getLeafsContainer().add(leaf.getDisplay());
			}
		}
	}

	private MediaFilePresenter createBrowserLeafInstance(final MediaFile file) {
		MediaFilePresenter.Display browserLeafView = display.newBrowserLeafView();
		final MediaFilePresenter leafPresenter = new MediaFilePresenter(eventBus, file,
				browserLeafView);
		leafPresenter.go();

		browserLeafView.getBtnIcon().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				openLeaf(leafPresenter.getFile());
			}
		});
		browserLeafView.getBtnEnqueue().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				enqueueSelectedLeaf(leafPresenter.getFile());
			}
		});
		browserLeafView.getBtnPlay().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				playLeaf(leafPresenter.getFile());
			}
		});

		return leafPresenter;
	}
	
	protected void playLeaf(MediaFile file) {
		if (MediaFile.TYPE.file.toString().equals(file.getType())) {
			playerPresenter.rpcSendCommand(PlayerApi.addPlayFile(file.getPath()));
		}
		else {
			playerPresenter.rpcSendCommand(PlayerApi.addPlayDir(file.getPath()));
		}
	}
	
	protected void enqueueSelectedLeaf(MediaFile file) {
		if (MediaFile.TYPE.file.toString().equals(file.getType())) {
			playerPresenter.rpcSendCommand(PlayerApi.addEnqueueFile(file.getPath()));
		}
		else {
			playerPresenter.rpcSendCommand(PlayerApi.addEnqueueDir(file.getPath()));
		}
	}
	
	private void requestBrowseMedia() {
		requestBrowseMedia("");
	}
	
	protected void requestBrowseMedia(String path) {				
		playerPresenter.getMediaFiles(path);	
	}
	
	protected void refreshBrowseMedia(String path) {				
		playerPresenter.refreshMediaFiles(path);	
	}
	
}
