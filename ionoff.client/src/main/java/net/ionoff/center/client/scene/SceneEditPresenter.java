package net.ionoff.center.client.scene;


import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.addins.client.combobox.MaterialComboBox;
import gwt.material.design.client.ui.MaterialCollapsible;
import gwt.material.design.client.ui.html.OptGroup;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.AppToken;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SceneDeviceDto;
import net.ionoff.center.shared.dto.SceneDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class SceneEditPresenter extends AbstractEditPresenter<SceneDto> {

	public interface Display extends IEditView<SceneDto> {

		MaterialComboBox<String> getListBoxZones();
		FlowPanel getSceneDevicesView();
	}
	
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private SceneDto entityDto;
	private SceneTablePresenter sceneManager;
	private List<SceneDevicePresenter> sceneDevicePresenters;

	public SceneEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, SceneTablePresenter sceneManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.sceneManager = sceneManager;
		sceneDevicePresenters = new ArrayList<>();
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
				sceneManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				sceneManager.hideEditForm();
			}
		});
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
		entityDto.setName(newName);
		
		if (entityDto.isNew()) {
			String selectedValue = view.getListBoxZones().getSelectedValue().get(0).toString();
			entityDto.setZoneId(BaseDto.parseIdFromFormattedNameID(selectedValue));
			entityDto.setZoneName(BaseDto.parseNameFromFormattedNameID(selectedValue));
			rpcSaveScene();
		}
		else {
			int edited = 0;
			for (SceneDevicePresenter sceneDevicePresenter : sceneDevicePresenters) {
				if (sceneDevicePresenter.isEdited()) {
					edited ++;
				}
			}
			final int totalRequest = edited;
			MethodCallback<Integer> callback = new MethodCallback<Integer>() {
				int successFullRequest = 0;
				@Override
				public void onSuccess(Method method, Integer response) {
					successFullRequest ++;
					if (successFullRequest == totalRequest) {
						rpcSaveScene();
					}
				}
				@Override
				public void onFailure(Method method, Throwable exception) {
					//
				}
			};
			for (SceneDevicePresenter sceneDevicePresenter : sceneDevicePresenters) {
				sceneDevicePresenter.save(callback);
			}
		}
	}
	
	private void rpcSaveScene() {
		rpcProvider.getSceneService().save(entityDto.getId(), entityDto, 
				new MethodCallback<SceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SceneDto result) {
				sceneManager.onSavedSucess(result);
			}
		});
	}
	

	@Override
	protected String getClazz() {
		return SceneDto.class.getSimpleName();
	}

	@Override
	protected EntityService<SceneDto> getRpcService() {
		return rpcProvider.getSceneService();
	}

	public void setEntityDto(SceneDto dto) {
		entityDto = dto;
		updateView(dto);
		loadAreaAndZonesByProjectId();
	}

	private void loadAreaAndZonesByProjectId() {
		rpcProvider.getAreaService().findByProjectId(AppToken.getProjectIdLong(), true, false, 
				new MethodCallback<List<AreaDto>>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				setZoneOptions(Collections.emptyList());
				setSceneDevices(null);
			}
			@Override
			public void onSuccess(Method method, List<AreaDto> result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setZoneOptions(result);
				if (entityDto.getId() != BaseDto.DEFAULT_ID && result != null && !result.isEmpty()) {
					loadSceneDevices();
				}
			}
		});
	}

	protected void loadSceneDevices() {
		rpcProvider.getSceneService().findById(entityDto.getId(), new MethodCallback<SceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
				setZoneOptions(Collections.emptyList());
				setSceneDevices(null);
			}
			@Override
			public void onSuccess(Method method, SceneDto result) {
				eventBus.fireEvent(ShowLoadingEvent.getInstance(false));
				setSceneDevices(result.getDevices());
			}
		});
	}
	

	public void setSceneDevices(List<SceneDeviceDto> devices) {
		sceneDevicePresenters.clear();
		view.getSceneDevicesView().clear();
		if (devices == null) {
			return;
		}
		MaterialCollapsible devicesCollapsible = new MaterialCollapsible();
		devicesCollapsible.setAccordion(false);
		view.getSceneDevicesView().add(devicesCollapsible);
		
		for (SceneDeviceDto sceneDevice :devices) {
			final SceneDeviceView sceneDeviceView = new SceneDeviceView(sceneDevice);
			SceneDevicePresenter sceneDevicePresenter 
					= new SceneDevicePresenter(rpcProvider, eventBus, sceneDevice, sceneDeviceView);
			sceneDevicePresenter.go();
			sceneDevicePresenters.add(sceneDevicePresenter);
			devicesCollapsible.add(sceneDeviceView);
		}
	}
	

	private void updateView(SceneDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		if (dto.getId() != BaseDto.DEFAULT_ID) {
			view.getListBoxZones().setEnabled(false);
			String zoneIdName = BaseDto.formatNameID(dto.getZoneName(), dto.getZoneId());
			view.getListBoxZones().setSingleValue(zoneIdName);
		}
		else {
			view.getListBoxZones().setEnabled(true);
			if (view.getListBoxZones().getValues().size() > 0) {
				view.getListBoxZones().setSelectedIndex(0);
			}
			else {
				view.getListBoxZones().setSelectedIndex(-1);
			}
		}
		if (dto.getId() == BaseDto.DEFAULT_ID) {
			view.getSceneDevicesView().setVisible(false);
		}
		else {
			view.getSceneDevicesView().setVisible(true);
		}
	}
	
	public void setZoneOptions(List<AreaDto> areas) {
		view.getListBoxZones().clear();
		if (areas == null || areas.isEmpty()) {
			return;
		}
		for (final AreaDto area : areas) {
			OptGroup group = new OptGroup(area.formatNameID());
			view.getListBoxZones().add(group);
			if (area.hasZone()) {
				for (ZoneDto zone : area.getZones()) {
					view.getListBoxZones().addItem(zone.formatNameID());
				}
			}
		}
		if (entityDto != null) {
			if (entityDto.getId() == BaseDto.DEFAULT_ID) {
				view.getListBoxZones().setSelectedIndex(0);
			}
			else {
				String value = BaseDto.formatNameID(entityDto.getZoneName(), 
						entityDto.getZoneId());
				view.getListBoxZones().setSingleValue(value);
			}
		}
	}
}
