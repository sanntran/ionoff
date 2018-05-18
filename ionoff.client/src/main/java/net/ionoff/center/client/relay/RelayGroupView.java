package net.ionoff.center.client.relay;

import java.util.ArrayList;
import java.util.List;

import com.google.gwt.dom.client.Style.Float;
import com.google.gwt.user.client.ui.FlowPanel;

import gwt.material.design.client.constants.Color;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLabel;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;

public class RelayGroupView extends MaterialCollapsible implements RelayGroupPresenter.Display {

	private final List<RelayItemView> relayViews;
	private final MaterialLabel lblName;
	private final MaterialIcon iconDelete;
	private final FlowPanel relayListPanel;
	private final RelaySelectionView relaySelectionView;
	
	private final MaterialButton btnAddRelay;
	
	public RelayGroupView() {
		
		addStyleName("relayGroup");
		relayViews = new ArrayList<>();
		
		MaterialCollapsibleItem collapsibleItem = new MaterialCollapsibleItem();
		MaterialCollapsibleHeader collapsibleHeader = new MaterialCollapsibleHeader();
		collapsibleItem.add(collapsibleHeader);
		
		iconDelete = new MaterialIcon();
		iconDelete.addStyleName("icon delete");
		iconDelete.setWaves(WavesType.DEFAULT);
		iconDelete.setIconType(IconType.DELETE);
		iconDelete.setFloat(Float.RIGHT);
		add(iconDelete);
		
		lblName = new MaterialLabel();
		lblName.setFontSize("15px");
		collapsibleHeader.add(lblName);
		
		MaterialCollapsibleBody collapsibleBody = new MaterialCollapsibleBody();
		collapsibleItem.add(collapsibleBody);

		relayListPanel = new FlowPanel();
		relayListPanel.setStyleName("modeSensorScenes");
		collapsibleBody.add(relayListPanel);
		
		relaySelectionView = new RelaySelectionView();
		collapsibleBody.add(relaySelectionView);
		
		btnAddRelay = new MaterialButton(ProjectLocale.getProjectConst().add() 
				+ " " + AdminLocale.getAdminConst().relay());
		btnAddRelay.setBackgroundColor(Color.WHITE);
		btnAddRelay.setTextColor(Color.GREY_DARKEN_4);
		btnAddRelay.setWaves(WavesType.DEFAULT);
		btnAddRelay.addStyleName("add");
		
		collapsibleBody.add(btnAddRelay);
		
		add(collapsibleItem);
	}

	public List<RelayItemView> getRelayViews() {
		return relayViews;
	}
	
	@Override
	public MaterialLabel getLblName() {
		return lblName;
	}
	
	@Override
	public MaterialIcon getBtnDelete() {
		return iconDelete;
	}
	
	@Override
	public FlowPanel getRelayListPanel() {
		return relayListPanel;
	}

	@Override
	public MaterialCollapsible asPanel() {
		return this;
	}

	@Override
	public MaterialButton getBtnAddRelay() {
		return btnAddRelay;
	}

	@Override
	public RelaySelectionView getRelaySelectionView() {
		return relaySelectionView;
	}
}