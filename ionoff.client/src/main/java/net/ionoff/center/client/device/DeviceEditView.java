package net.ionoff.center.client.device;

import com.google.gwt.user.client.ui.InlineLabel;
import com.google.gwt.user.client.ui.Label;

import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleHeader;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import gwt.material.design.client.ui.html.Option;
import net.ionoff.center.client.base.AbstractEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.PlayerDto;

public class DeviceEditView extends AbstractEditView<DeviceDto> implements DeviceEditPresenter.Display {
	
	private MaterialListBox listBoxTypes;
	private MaterialTextBox textBoxOrder;
	private MaterialTextBox textBoxMac;
	private MaterialTextBox textBoxIp;
	private MaterialListBox listBoxModels;
	
	private MaterialCollapsible zoneCollapsible;
	private MaterialLabel zoneLbl;
	private MaterialCollapsibleItem zoneCollapsibleItem;
	private MaterialCollapsibleBody zoneCollapsibleBody;
	
	public DeviceEditView() {
		super();
		
		getLblIcon().setIconType(IconType.DEVICES_OTHER);
		
		Label lblType = new InlineLabel(AdminLocale.getAdminConst().type());
		lblType.setStyleName("lbl");
		contentPanel.add(lblType);
		listBoxTypes = new MaterialListBox();
		listBoxTypes.addItem(AdminLocale.getAdminConst().light());
		listBoxTypes.addItem(AdminLocale.getAdminConst().mediaPlayer());
		listBoxTypes.addItem(AdminLocale.getAdminConst().weighScale());
		listBoxTypes.addItem(AdminLocale.getAdminConst().appliance());
		contentPanel.add(listBoxTypes);
		
		textBoxMac = new MaterialTextBox();
		textBoxMac.setPlaceholder(AdminLocale.getAdminConst().mac());
		contentPanel.add(textBoxMac);
		
		textBoxIp = new MaterialTextBox();
		textBoxIp.setPlaceholder(AdminLocale.getAdminConst().ip());
		contentPanel.add(textBoxIp);
		
		listBoxModels = new MaterialListBox();
		listBoxModels.setPlaceholder(AdminLocale.getAdminConst().model());
		listBoxModels.add(new Option(PlayerDto.XMP));
		listBoxModels.add(new Option(PlayerDto.IMP));
		contentPanel.add(listBoxModels);
		
		textBoxOrder = new MaterialTextBox();
		textBoxOrder.setLabel(AdminLocale.getAdminConst().order());
		contentPanel.add(textBoxOrder);
		
		Label lblZone = new InlineLabel(AdminLocale.getAdminConst().zone());
		lblZone.setStyleName("lbl");
		contentPanel.add(lblZone);
		
		zoneCollapsible = new MaterialCollapsible();
		contentPanel.add(zoneCollapsible);
		
		zoneCollapsibleItem = new MaterialCollapsibleItem();
		MaterialCollapsibleHeader zoneCollapsibleHeader = new MaterialCollapsibleHeader();
		zoneCollapsibleItem.add(zoneCollapsibleHeader);
		zoneLbl = new MaterialLabel();
		zoneLbl.setFontSize("15px");
		zoneCollapsibleHeader.add(zoneLbl);
		
		zoneCollapsibleBody = new MaterialCollapsibleBody();
		zoneCollapsibleItem.add(zoneCollapsibleBody);
		
		zoneCollapsible.add(zoneCollapsibleItem);
	}

	@Override
	public MaterialTextBox getTextBoxOrder() {
		return textBoxOrder;
	}
	
	@Override
	public MaterialTextBox getTextBoxMac() {
		return textBoxMac;
	}
	
	@Override
	public MaterialListBox getListBoxTypes() {
		return listBoxTypes;
	}
	
	@Override
	public MaterialCollapsible getZoneCollapsible() {
		return zoneCollapsible;
	}
	
	@Override
	public MaterialCollapsibleItem getZoneCollapsibleItem() {
		return zoneCollapsibleItem;
	}
	
	@Override
	public MaterialLabel getZoneNameLbl() {
		return zoneLbl;
	}
	
	@Override
	public MaterialCollapsibleBody getZoneCollapsibleBody() {
		return zoneCollapsibleBody;
	}

	@Override
	public MaterialTextBox getTextBoxIp() {
		return textBoxIp;
	}

	@Override
	public MaterialListBox getListBoxModels() {
		return listBoxModels;
	}

}
