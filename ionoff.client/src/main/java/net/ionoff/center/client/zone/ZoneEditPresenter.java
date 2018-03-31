package net.ionoff.center.client.zone;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialIntegerBox;
import gwt.material.design.client.ui.MaterialListBox;
import net.ionoff.center.client.common.AbstractEditPresenter;
import net.ionoff.center.client.common.IEditView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.AreaDto;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ZoneDto;

public class ZoneEditPresenter extends AbstractEditPresenter<ZoneDto> {

	public interface Display extends IEditView<ZoneDto> {
		MaterialIntegerBox getIntBoxOrder();
		MaterialListBox getListBoxAreas();
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private ZoneDto entityDto;
	private ZoneTablePresenter zoneManager;

	public ZoneEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, ZoneTablePresenter zoneManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.zoneManager = zoneManager;
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
				zoneManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				zoneManager.hideEditForm();
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
		int selectedAreaIndex = view.getListBoxAreas().getSelectedIndex();
		if (selectedAreaIndex < 0) {
			final String message = AdminLocale.getAdminMessages().invalidFieldValue(AdminLocale.getAdminConst().area());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return;
		}
		
		entityDto.setName(newName);
		entityDto.setOrder(view.getIntBoxOrder().getValue());
		
		String selectedItem = view.getListBoxAreas().getItemText(selectedAreaIndex);
		entityDto.setAreaId(BaseDto.parseIdFromFormattedNameID(selectedItem));
		entityDto.setAreaName(BaseDto.parseNameFromFormattedNameID(selectedItem));
		
		rpcProvider.getZoneService().save(entityDto.getId(), entityDto, 
				new MethodCallback<ZoneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ZoneDto result) {
				zoneManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return ZoneDto.class.getSimpleName();
	}

	@Override
	protected EntityService<ZoneDto> getRpcService() {
		return rpcProvider.getZoneService();
	}

	public void setEntityDto(ZoneDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(ZoneDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getIntBoxOrder().setValue(entityDto.getOrder());
		if (dto.getId() != BaseDto.DEFAULT_ID) {
			view.getListBoxAreas().setSelectedValue(BaseDto.formatNameID(dto.getAreaName(), dto.getAreaId()));
		}
		else {
			if (view.getListBoxAreas().getItemCount() > 0) {
				view.getListBoxAreas().setSelectedIndex(0);
			}
			else {
				view.getListBoxAreas().setSelectedIndex(-1);
			}
		}
	}
 
	public void setAreaOptions(List<AreaDto> options) {
		view.getListBoxAreas().clear();
		if (options == null || options.isEmpty()) {
			return;
		}
		for (final AreaDto option : options) {
			view.getListBoxAreas().addItem(option.formatNameID());
		}
	}
}
