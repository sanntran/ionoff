package net.ionoff.center.client.zone;

import com.google.gwt.core.client.GWT;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCardContent;
import gwt.material.design.client.ui.MaterialCardTitle;
import gwt.material.design.client.ui.MaterialLabel;

public class ZoneCard extends Composite {

	@UiTemplate("ZoneCard.ui.xml")
	interface ZoneCardUiBinder extends UiBinder<Widget, ZoneCard> {}
	static ZoneCardUiBinder uiBinder = GWT.create(ZoneCardUiBinder.class);

	@UiField
	MaterialCard root;
	@UiField
	MaterialCardContent cardContent;
	@UiField
	MaterialCardTitle cardTitle;
	@UiField
	MaterialLabel lblName;
	@UiField
	MaterialLabel lblDescription;
	@UiField
	MaterialLabel lblDetail;

	public ZoneCard() {
		initWidget(uiBinder.createAndBindUi(this));
		cardTitle.setText("");
	}

	public void setTitle(String title) {
		cardTitle.setText(title);
	}

	public ZoneCard setTextColor(Color textColor) {
		cardContent.setTextColor(textColor);
		lblName.setTextColor(textColor);
		lblDescription.setTextColor(textColor);
		lblDetail.setTextColor(textColor);
		return this;
	}

	public ZoneCard setIconType(IconType icon) {
		cardTitle.setIconType(icon);
		return this;
	}

	public ZoneCard setName(String name) {
		lblName.setText(name);
		return this;
	}

	public ZoneCard setDetail(String detail) {
		lblDetail.setText(detail);
		return this;
	}

	public void setDescription(String description) {
		lblDescription.setText(description);
	}

	public void setBackgroundColor(Color color) {
		root.setBackgroundColor(color);
	}

}
