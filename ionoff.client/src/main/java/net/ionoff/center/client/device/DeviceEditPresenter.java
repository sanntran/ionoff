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
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.client.utils.ConsoleLog;
import net.ionoff.center.shared.dto.ApplianceDto;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.DeviceDto;
import net.ionoff.center.shared.dto.LightDto;
import net.ionoff.center.shared.dto.PlayerDto;
import net.ionoff.center.shared.dto.WeighScaleDto;
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
				if (typeIndex == 0) {
					setEntityDto(newLightDto(entityDto));
				}
				else if (typeIndex == 1) {
					setEntityDto(newPlayerDto(entityDto));
				}
				else if (typeIndex == 2) {
					setEntityDto(newWeighScaleDto(entityDto));
				}
				else {
					setEntityDto(newApplianceDto(entityDto));
				}
				view.getListBoxTypes().setSelectedIndex(typeIndex);
			}
		});
	}
	
	private ApplianceDto newApplianceDto(DeviceDto target) {
		ApplianceDto applicanceDto = new ApplianceDto();
		copyDeviceDto(target, applicanceDto);
		return applicanceDto;
	}
	
	private LightDto newLightDto(DeviceDto target) {
		LightDto lightDto = new LightDto();
		copyDeviceDto(target, lightDto);
		return lightDto;
	}

	private PlayerDto newPlayerDto(DeviceDto target) {
		PlayerDto playerDto = new PlayerDto();
		copyDeviceDto(target, playerDto);
		playerDto.setModel(PlayerDto.IMP);
		playerDto.setIp("");
		return playerDto;
	}
	
	private WeighScaleDto newWeighScaleDto(DeviceDto target) {
		WeighScaleDto weighScaleDto = new WeighScaleDto();
		copyDeviceDto(target, weighScaleDto);
		return weighScaleDto;
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
		
		if (entityDto instanceof PlayerDto) {
			PlayerDto player = (PlayerDto)entityDto;
			player.setMac(view.getTextBoxMac().getValue());
			player.setModel(view.getListBoxModels().getSelectedValue());
			player.setIp(view.getTextBoxIp().getText());
		}
		
		else if (entityDto instanceof WeighScaleDto) {
			WeighScaleDto scale = (WeighScaleDto)entityDto;
			scale.setMac(view.getTextBoxMac().getValue());
			scale.setModel(view.getListBoxModels().getSelectedValue());
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
		if (dto instanceof PlayerDto) {
			PlayerDto player = (PlayerDto)dto;
			view.getTextBoxMac().setText(player.getMac());
			view.getTextBoxMac().setVisible(true);
			view.getTextBoxIp().setVisible(true);
			view.getTextBoxIp().setText(player.getIp());
			view.getListBoxModels().setVisible(true);
			view.getListBoxModels().setSelectedValue(player.getModel());
		}
		else if (dto instanceof WeighScaleDto) {
			WeighScaleDto scale = (WeighScaleDto)dto;
			view.getTextBoxMac().setText(scale.getMac());
			view.getTextBoxMac().setVisible(true);
			view.getTextBoxIp().setVisible(false);
			view.getListBoxModels().setVisible(false);
		}
		else {
			view.getTextBoxIp().setVisible(false);
			view.getListBoxModels().setVisible(false);
			view.getTextBoxMac().setText("");
			view.getTextBoxMac().setVisible(false);
		}
		if (dto instanceof LightDto) {
			view.getListBoxTypes().setSelectedIndex(0);
		}
		else if (dto instanceof PlayerDto) {
			view.getListBoxTypes().setSelectedIndex(1);
		}
		else if (dto instanceof WeighScaleDto) {
			view.getListBoxTypes().setSelectedIndex(2);
		}
		else {
			view.getListBoxTypes().setSelectedIndex(3);
		}
		view.getZoneNameLbl().setText(BaseDto.formatNameID(dto.getZoneName(), dto.getZoneId()));
		
	}
	
	private String getTypeName(DeviceDto dto) {
		if (dto instanceof LightDto) {
			return AdminLocale.getAdminConst().light();
		}
		else if (dto instanceof PlayerDto) {
			return AdminLocale.getAdminConst().mediaPlayer();
		}
		else if (dto instanceof WeighScaleDto) {
			return AdminLocale.getAdminConst().weighScale();
		}
		else {
			return AdminLocale.getAdminConst().appliance();
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
