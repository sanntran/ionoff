package net.ionoff.center.client.area;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.*;
import net.ionoff.center.shared.dto.AreaCellDto;

import static gwt.material.design.client.constants.Color.RED_ACCENT_2;
import static net.ionoff.center.client.locale.ProjectLocale.getProjectConst;

public class AreaCell extends Composite {

	@UiTemplate("AreaCell.ui.xml")
	interface AreaCellUiBinder extends UiBinder<Widget, AreaCell> {}
	static AreaCellUiBinder uiBinder = GWT.create(AreaCellUiBinder.class);

	@UiField
	MaterialColumn root;
	@UiField
	MaterialCard card;
	@UiField
	MaterialCardContent cardContent;
	@UiField
	MaterialCardTitle cardTitle;
	@UiField
	MaterialLabel lblZoneCount;
	@UiField
	MaterialLabel lblAlertCount;
	@UiField
	MaterialLabel lblAlertDetail;

	public AreaCell(AreaCellDto area) {
		initWidget(uiBinder.createAndBindUi(this));
		cardTitle.setText(area.getName());
		lblZoneCount.setText(area.getZoneCount() + " " + getProjectConst().zone());
		lblAlertCount.setText(area.getAlertCount() + " " +  getProjectConst().alert());
		if (area.hasAlert()) {
			card.setBackgroundColor(RED_ACCENT_2);
		}
	}

	public void setTitle(String title) {
		cardTitle.setText(title);
	}

	public AreaCell setIconType(IconType icon) {
		cardTitle.setIconType(icon);
		return this;
	}

}
