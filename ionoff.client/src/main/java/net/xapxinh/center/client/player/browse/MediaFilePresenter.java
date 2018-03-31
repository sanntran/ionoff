package net.xapxinh.center.client.player.browse;

import net.xapxinh.center.client.player.AbstractPresenter;
import net.xapxinh.center.shared.dto.MediaFile;

import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.Button;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.user.client.ui.Label;

/**
 * @author Sann Tran
 */
public class MediaFilePresenter extends AbstractPresenter {

	public interface Display {

		FlowPanel asWidget();

		Button getBtnIcon();
		
		Label getLblName();
		
		Button getBtnEnqueue();

		Button getBtnPlay();
	}

	private final Display display;

	private MediaFile fileDto;

	public MediaFilePresenter(HandlerManager eventBus, MediaFile file, Display view) {
		
		super(eventBus);
		this.display = view;
		this.fileDto = file;
	}

	public void bind() {
		String iconStyle = "gwt-Button file";
		if ("dir".equalsIgnoreCase(fileDto.getType())) {
			iconStyle = "gwt-Button dir";
		}
		display.getBtnIcon().setStyleName(iconStyle);

		String name = fileDto.getName();
		if (name == null) {
			display.getLblName().setText("N/a");
		}
		if (name.equals("..")) {
			display.getBtnPlay().setVisible(false);
			display.getBtnEnqueue().setVisible(false);
		}
		display.getLblName().setText(name);
	}

	public MediaFile getFile() {
		return fileDto;
	}

	public FlowPanel getDisplay() {
		return display.asWidget();
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		
	}
}
