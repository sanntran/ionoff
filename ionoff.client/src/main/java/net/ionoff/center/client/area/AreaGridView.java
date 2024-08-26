package net.ionoff.center.client.area;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.HTMLPanel;
import com.google.gwt.user.client.ui.Panel;
import com.google.gwt.user.client.ui.Widget;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialRow;
import net.ionoff.center.shared.dto.AreaCellDto;

import java.util.List;

import static net.ionoff.center.client.locale.ProjectLocale.getProjectConst;

public class AreaGridView extends Composite implements AreaGridPresenter.Display {

	@UiTemplate("AreaGridView.ui.xml")
	interface AreaGridViewUiBinder extends UiBinder<Widget, AreaGridView> {}
	static AreaGridView.AreaGridViewUiBinder uiBinder = GWT.create(AreaGridView.AreaGridViewUiBinder.class);

	@UiField
	HTMLPanel root;
	@UiField
	MaterialRow header;
	@UiField
	MaterialLink lblTitle;
	@UiField
	MaterialRow content;

	public AreaGridView() {
		uiBinder.createAndBindUi(this);
		lblTitle.setText(getProjectConst().area());
	}

	@Override
	public void setPresenter(AreaGridPresenter presenter) {

	}

	@Override
	public void setAreas(List<AreaCellDto> areas) {
		this.content.clear();
		for (AreaCellDto area : areas) {
			AreaCell areaCell = new AreaCell(area);
			content.add(areaCell);
		}
	}

	@Override
	public Panel asWidget() {
		return this.root;
	}
}
