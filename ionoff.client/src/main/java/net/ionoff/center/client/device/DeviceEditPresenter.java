package net.ionoff.center.client.device;


import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.logical.shared.ValueChangeEvent;
import com.google.gwt.event.logical.shared.ValueChangeHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.MaterialCollapsibleBody;
import gwt.material.design.client.ui.MaterialCollapsibleItem;
import gwt.material.design.client.ui.MaterialCollection;
import gwt.material.design.client.ui.MaterialCollectionItem;
import gwt.material.design.client.ui.MaterialLabel;
import gwt.material.design.client.ui.MaterialListBox;
import gwt.material.design.client.ui.MaterialTextBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.utils.ConsoleLog;
import net.ionoff.center.shared.dto.RelayLoadDto;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.MediaPlayerDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class DeviceEditPresenter extends AbstractEditPresenter<DeviceDto> {

	public interface Display extends IEditView<DeviceDto> {
		MaterialTextBox getTextBoxOrder();

		MaterialTextBox getTextBoxMac();
		
		MaterialTextBox getTextBoxIp();

		MaterialListBox getListBoxTypes();
		
		MaterialListBox getListBoxModels();

		MaterialLabel getZoneNameLbl();

		MaterialCollapsibleBody getZoneCollapsibleBody();

		MaterialCollapsibleItem getZoneCollapsibleItem();

		MaterialCollapsible getZoneCollapsible();
		
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private DeviceDto entityDto;
	private DeviceTablePresenter deviceManager;
	private final List<ZoneDto> zoneDtos;

	public DeviceEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, DeviceTablePresenter deviceManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.deviceManager = deviceManager;
		zoneDtos = new ArrayList<>();
	}

	@Override
	protected void bind() {
		view.getBtnSave().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				save();
			}
		});
		view.getBtnClose().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				deviceManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				deviceManager.hideEditForm();
			}
		});
		view.getListBoxTypes().addValueChangeHandler(new ValueChangeHandler<String>() {
			@Override
			public void onValueChange(ValueChangeEvent<String> event) {
				entityDto.setName(display.getTextBoxName().getText());
				int typeIndex = view.getListBoxTypes().getSelectedIndex();
				ConsoleLog.println(typeIndex + "");
				if (typeIndex == 1) {
					setEntityDto(newMediaPlayerrDto(entityDto));
				}
				else {
					setEntityDto(newRelayLoadDto(entityDto));
				}
				view.getListBoxTypes().setSelectedIndex(typeIndex);
			}
		});
	}
	
	private RelayLoadDto newRelayLoadDto(DeviceDto target) {
		RelayLoadDto relayLoadDto = new RelayLoadDto();
		copyDeviceDto(target, relayLoadDto);
		return relayLoadDto;
	}

	private MediaPlayerDto newMediaPlayerrDto(DeviceDto target) {
		MediaPlayerDto playerDto = new MediaPlayerDto();
		copyDeviceDto(target, playerDto);
		playerDto.setModel(MediaPlayerDto.IMP);
		playerDto.setIp("");
		return playerDto;
	}

	private void copyDeviceDto(DeviceDto sourceDevice, DeviceDto targetDevice) {
		targetDevice.setId(sourceDevice.getId());
		targetDevice.setName(sourceDevice.getName());
		targetDevice.setOrder(sourceDevice.getOrder());
		targetDevice.setZoneId(sourceDevice.getZoneId());
		targetDevice.setZoneName(sourceDevice.getZoneName());
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		//
	}

	@Override
	protected void save() {
		if (entityDto == null) {
			return;
		}
		String newName = view.getTextBoxName().getValue();
		if (!validateInputStringValue(AdminLocale.getAdminConst().name(), newName)) {
			return;
		}
		String newOrder = view.getTextBoxOrder().getValue();
		if (!validateInputNumberValue(AdminLocale.getAdminConst().order(), newOrder)) {
			return;
		}
		
		entityDto.setName(newName);
		entityDto.setOrder(Integer.parseInt(newOrder));
		
		if (entityDto instanceof MediaPlayerDto) {
			MediaPlayerDto player = (MediaPlayerDto)entityDto;
			player.setMac(view.getTextBoxMac().getValue());
			player.setModel(view.getListBoxModels().getSelectedValue());
			player.setIp(view.getTextBoxIp().getText());
		}

		rpcProvider.getDeviceService().save(entityDto.getId(), entityDto, 
				new MethodCallback<DeviceDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, DeviceDto result) {
				deviceManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return DeviceDto.class.getSimpleName();
	}

	@Override
	protected EntityService<DeviceDto> getRpcService() {
		return rpcProvider.getDeviceService();
	}

	public void setEntityDto(DeviceDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(DeviceDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		if (dto.getOrder() == null) {
			view.getTextBoxOrder().setText("0");
		}
		else {
			view.getTextBoxOrder().setText(dto.getOrder() + "");
		}
		if (dto.getId() != BaseDto.DEFAULT_ID) {
			view.getListBoxTypes().setEnabled(false);
			view.getListBoxTypes().setSelectedValue(getTypeName(dto));
		}
		else {
			view.getListBoxTypes().setEnabled(true);
			if (view.getListBoxTypes().getItemCount() > 0) {
				view.getListBoxTypes().setSelectedIndex(0);
			}
			else {
				view.getListBoxTypes().setSelectedIndex(-1);
			}
		}
		if (dto instanceof MediaPlayerDto) {
			MediaPlayerDto player = (MediaPlayerDto)dto;
			view.getTextBoxMac().setText(player.getMac());
			view.getTextBoxMac().setVisible(true);
			view.getTextBoxIp().setVisible(true);
			view.getTextBoxIp().setText(player.getIp());
			view.getListBoxModels().setVisible(true);
			view.getListBoxModels().setSelectedValue(player.getModel());
		}
		else {
			view.getTextBoxIp().setVisible(false);
			view.getListBoxModels().setVisible(false);
			view.getTextBoxMac().setText("");
			view.getTextBoxMac().setVisible(false);
		}
		if (dto instanceof MediaPlayerDto) {
			view.getListBoxTypes().setSelectedIndex(1);
		}
		if (dto instanceof RelayLoadDto) {
			view.getListBoxTypes().setSelectedIndex(0);
		}
		view.getZoneNameLbl().setText(BaseDto.formatNameID(dto.getZoneName(), dto.getZoneId()));
		
	}
	
	private String getTypeName(DeviceDto dto) {
		if (dto instanceof MediaPlayerDto) {
			return AdminLocale.getAdminConst().mediaPlayer();
		}
		else {
			return AdminLocale.getAdminConst().relayLoad();
		}
	}

	public void setZoneOptions(List<AreaDto> areaDtos) {
		zoneDtos.clear();
		view.getZoneCollapsibleBody().clear();
		MaterialCollection zoneCollection = new MaterialCollection();
		view.getZoneCollapsibleBody().add(zoneCollection);
		
		for (AreaDto area : areaDtos) {
			for (final ZoneDto zone : area.getZones()) {
				zoneDtos.add(zone);
				MaterialCollectionItem zoneItem = new MaterialCollectionItem();
				MaterialLabel zonelbl = new MaterialLabel(zone.formatNameID());
				zonelbl.setFontSize("16px");
				zoneItem.add(zonelbl);
				MaterialLabel areaLbl = new MaterialLabel(area.formatNameID());
				zoneItem.add(areaLbl);
				
				zoneCollection.add(zoneItem);
				
				zoneItem.addClickHandler(new ClickHandler() {
					
					@Override
					public void onClick(ClickEvent event) {
						view.getZoneNameLbl().setText(zone.formatNameID());
						entityDto.setZoneId(zone.getId());
						entityDto.setZoneName(zone.getName());
						view.getZoneCollapsibleItem().collapse();
						view.getZoneCollapsible().clearActive();
					}
				});
			}
		}
	}
	
	public List<ZoneDto> getZoneDtos() {
		return zoneDtos;
	}
}
