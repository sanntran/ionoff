package net.ionoff.center.client.content;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;

public class ContentView extends Composite implements ContentPresenter.Display {

	@UiTemplate("ContentView.ui.xml")
	interface ContentViewUiBinder extends UiBinder<Widget, ContentView> {
	}

	private static ContentViewUiBinder uiBinder = GWT.create(ContentViewUiBinder.class);

	@UiField
	HTMLPanel root;


	public ContentView() {
		uiBinder.createAndBindUi(this);
	}


	@Override
	public Panel asPanel() {
		return this.root;
	}
}
