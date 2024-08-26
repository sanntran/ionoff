package net.ionoff.center.client.ui;

import com.google.gwt.core.client.GWT;
import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.uibinder.client.UiBinder;
import com.google.gwt.uibinder.client.UiField;
import com.google.gwt.uibinder.client.UiHandler;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Composite;
import com.google.gwt.user.client.ui.Widget;
import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCard;
import gwt.material.design.client.ui.MaterialCardContent;
import gwt.material.design.client.ui.MaterialCardTitle;
import gwt.material.design.client.ui.MaterialLabel;

import java.util.Optional;

public class ProjectDashboardCard extends Composite {

	@UiTemplate("ProjectDashboardCard.ui.xml")
	interface DashboardCardUiBinder extends UiBinder<Widget, ProjectDashboardCard> {}
	static DashboardCardUiBinder uiBinder = GWT.create(DashboardCardUiBinder.class);

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

	ClickHandler clickHandler;

	public ProjectDashboardCard() {
		initWidget(uiBinder.createAndBindUi(this));
		cardTitle.setText("0");
	}

	@UiHandler("root")
	void onRootClicked(ClickEvent event) {
		Optional.ofNullable(clickHandler).ifPresent(clickHandler -> clickHandler.onClick(event));
	}

	public void setTitleValue(String value) {
		cardTitle.setText(value);
	}

	public void setClickHandler(ClickHandler clickHandler) {
		this.clickHandler = clickHandler;
	}

	public ProjectDashboardCard setTextColor(Color textColor) {
		cardContent.setTextColor(textColor);
		lblName.setTextColor(textColor);
		lblDescription.setTextColor(textColor);
		lblDetail.setTextColor(textColor);
		return this;
	}

	public ProjectDashboardCard setIconType(IconType icon) {
		cardTitle.setIconType(icon);
		return this;
	}

	public ProjectDashboardCard setName(String name) {
		lblName.setText(name);
		return this;
	}

	public ProjectDashboardCard setDetail(String detail) {
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
