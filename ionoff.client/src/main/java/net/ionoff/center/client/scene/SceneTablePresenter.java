package net.ionoff.center.client.scene;

import java.util.ArrayList;
import java.util.List;

import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;

import net.ionoff.center.client.common.AbstractTablePresenter;
import net.ionoff.center.client.common.ITableView;
import net.ionoff.center.client.event.ShowMessageEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.BaseDto;
import net.ionoff.center.shared.dto.SceneDto;
 
public class SceneTablePresenter extends AbstractTablePresenter<SceneDto>{
	
	public interface Display extends ITableView<SceneDto> {
		Column<SceneDto, String> getNameColumn();
		Column<SceneDto, String> getAreaColumn();
		SceneEditView getSceneEditView();
	}
	
	private final Display view;
	private final IRpcServiceProvider rpcProvider;
	private SceneEditPresenter sceneEditPresenter;
	
	public SceneTablePresenter(IRpcServiceProvider adminRpcProvider, HandlerManager eventBus,
			Display view) {
		super(adminRpcProvider, eventBus, view);
		this.rpcProvider = adminRpcProvider;
		this.view = view;
	}
	
	@Override
	public void bind() {
		super.bind();
		
		view.getEditColumn().setFieldUpdater(new FieldUpdater<SceneDto, String>() {
			@Override
			public void update(int index, SceneDto object, String value) {
				showEditForm();
			}
		});
	}

	@Override
	public void go() {
		bind();
	}

	@Override
	public void show(HasWidgets container) {
		container.clear();
		container.add(display.asWidget());
	}

	@Override
	protected boolean validateBeforeSaving() {
		if (!super.validateBeforeSaving()) {
			return false;
		}
		if (getUnsavedDto().getZoneId() == null) {
			String message = AdminLocale.getAdminMessages().emptyInputValue(AdminLocale.getAdminConst().zone());
			eventBus.fireEvent(new ShowMessageEvent(message, ShowMessageEvent.ERROR));
			return false;
		}
		return true;
	}
	
	@Override
	protected void save() {
		rpcProvider.getSceneService().save(getUnsavedDto().getId(), getUnsavedDto(), 
				new MethodCallback<SceneDto>() {
			@Override
			public void onFailure(Method method, Throwable exception) {
				ClientUtil.handleRpcFailure(method, exception, eventBus);
			}
			@Override
			public void onSuccess(Method method, SceneDto result) {
				onSavedSucess(result);
			}
		});
	}

	@Override
	protected void add() {
		List<SceneDto> sceneDtos = new ArrayList<SceneDto>();
		SceneDto newSceneDto = newSceneDto();
		sceneDtos.add(newSceneDto);
		sceneDtos.addAll(getDisplayingDtos());
		showEntities(sceneDtos, newSceneDto.getId(), 0);
		showEditForm();
		getSceneEditPresenter().setEntityDto(newSceneDto);
	}

	private SceneDto newSceneDto() {
		SceneDto newSceneDto = new SceneDto();
		newSceneDto.setId(BaseDto.DEFAULT_ID);
		newSceneDto.setName("*" + AdminLocale.getAdminConst().scene());
		return newSceneDto;
	}

	@Override
	protected AsyncDataProvider<SceneDto> newAsyncDataProvider() {
		final AsyncDataProvider<SceneDto> provider = new AsyncDataProvider<SceneDto>() {
			@Override
			protected void onRangeChanged(HasData<SceneDto> hasData) {
				final int start = hasData.getVisibleRange().getStart();
				// int length = hasData.getVisibleRange().getLength();
				loadData(null, false, start);
			}
		};
		return provider;
	}

	@Override
	protected void fillListBoxSearchBy() {
		display.getToolBarView().getLisBoxSearchBy().addItem(AdminLocale.getAdminConst().name());
		String areaName = AdminLocale.getAdminConst().name() + " " + AdminLocale.getAdminConst().area();
		display.getToolBarView().getLisBoxSearchBy().addItem(areaName);
	}

	@Override
	protected boolean isSameId(SceneDto entity, Long selectedId) {
		return entity.getId() == selectedId;
	}

	@Override
	protected String getClazz() {
		return SceneDto.class + "";
	}

	@Override
	protected String getSearchBy() {
		int selectedSearchByIndex = display.getToolBarView().getLisBoxSearchBy().getSelectedIndex();
		if (selectedSearchByIndex == 0) {
			return "name";
		}
		else {
			return "areaName";
		}
	}

	@Override
	protected EntityService<SceneDto> getRpcService() {
		return rpcProvider.getSceneService();
	}
	
	@Override
	protected String getSortByField(int columnIndex) {
		return BaseDto.NAME;
	}
	
	public void hideEditForm() {
		view.hideEditForm();
	}
	
	@Override
	protected void setSelectedObject(SceneDto selectedDto) {
		getSceneEditPresenter().setEntityDto(selectedDto);
	}

	private void showEditForm() {
		view.showEditForm();
	}
	
	public SceneEditPresenter getSceneEditPresenter() {
		if (sceneEditPresenter == null) {
			sceneEditPresenter = new SceneEditPresenter(rpcProvider, eventBus, view.getSceneEditView(), this);
			sceneEditPresenter.go();
		}
		return sceneEditPresenter;
	}
}
