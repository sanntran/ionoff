package net.ionoff.center.client.mode;


import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.event.dom.client.ClickEvent;
import com.google.gwt.event.dom.client.ClickHandler;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.client.ui.HasWidgets;

import gwt.material.design.client.ui.MaterialCheckBox;
import net.ionoff.center.client.base.AbstractEditPresenter;
import net.ionoff.center.client.base.IEditView;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.schedule.ScheduleTimeSettingPanel;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.ModeDto;
import net.ionoff.center.shared.dto.ModeSceneDto;

public class ModeEditPresenter extends AbstractEditPresenter<ModeDto> {

	public interface Display extends IEditView<ModeDto> {
		MaterialCheckBox getCheckBoxScheduled();
		ModeSceneListPresenter.Display getSceneListView();
		ScheduleTimeSettingPanel getScheduleTimeSettingPanel();
		void checkShowingScheduleTimeSettingPanel(Boolean isScheduled);
	}
	protected IRpcServiceProvider rpcProvider;
	
	private final Display view;
	private ModeDto entityDto;
	private ModeTablePresenter modeManager;
	private ModeSceneListPresenter modeScenesTabPresenter;

	public ModeEditPresenter(IRpcServiceProvider rpcProvider, 
			HandlerManager eventBus, Display view, ModeTablePresenter modeManager) {
		super(rpcProvider, eventBus, view);
		this.rpcProvider = rpcProvider;
		this.view = view;
		this.modeManager = modeManager;
		modeScenesTabPresenter = new ModeSceneListPresenter(rpcProvider, eventBus, view.getSceneListView());
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
				modeManager.hideEditForm();
			}
		});
		view.getBtnCancel().addClickHandler(new ClickHandler() {
			@Override
			public void onClick(ClickEvent event) {
				modeManager.hideEditForm();
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
		
		if (view.getCheckBoxScheduled().getValue() != null && view.getCheckBoxScheduled().getValue().booleanValue() == true) {
			entityDto.setIsScheduled(true);
			entityDto.setScheduleRepeat(view.getScheduleTimeSettingPanel().getSelectedRepeat());
			entityDto.setScheduleDay(view.getScheduleTimeSettingPanel().getSelectedDay());
			entityDto.setScheduleTime(view.getScheduleTimeSettingPanel().getSelectedTime());
		}
		else {
			entityDto.setIsScheduled(false);
		}
		
		rpcProvider.getModeService().save(entityDto.getId(), entityDto, 
				new MethodCallback<ModeDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, ModeDto result) {
				modeManager.onSavedSucess(result);
			}
		});
	}

	@Override
	protected String getClazz() {
		return ModeDto.class.getSimpleName();
	}

	@Override
	protected EntityService<ModeDto> getRpcService() {
		return rpcProvider.getModeService();
	}

	public void setEntityDto(ModeDto dto) {
		entityDto = dto;
		updateView(dto);
	}

	private void updateView(ModeDto dto) {
		view.getLblId().setText("#" + dto.getId());
		view.getLblName().setText(dto.getName());
		view.getTextBoxName().setText(dto.getName());
		view.getCheckBoxScheduled().setValue(dto.getIsScheduled());
		view.checkShowingScheduleTimeSettingPanel(dto.getIsScheduled());
		
		if (dto.getId() == BaseDto.DEFAULT_ID) {
			hideModeSceneListPanel();
		}
		else {
			showModeSceneListPanel(dto.getScenes());
		}
		view.getScheduleTimeSettingPanel().setScheduleData(dto.getScheduleRepeat(), 
				dto.getScheduleDay(), dto.getScheduleTime());
	}

	private void hideModeSceneListPanel() {
		view.getSceneListView().asPanel().setVisible(false);
	}

	private void showModeSceneListPanel(List<ModeSceneDto> modeScenes) {
		view.getSceneListView().asPanel().setVisible(true);
		modeScenesTabPresenter.setModeScenes(modeScenes);
	}
}
