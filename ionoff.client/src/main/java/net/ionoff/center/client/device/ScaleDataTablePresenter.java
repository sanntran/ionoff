package net.ionoff.center.client.device;

import com.google.gwt.cell.client.FieldUpdater;
import com.google.gwt.event.shared.HandlerManager;
import com.google.gwt.user.cellview.client.Column;
import com.google.gwt.user.client.ui.HasWidgets;
import com.google.gwt.view.client.AsyncDataProvider;
import com.google.gwt.view.client.HasData;
import net.ionoff.center.client.base.AbstractTablePresenter;
import net.ionoff.center.client.base.ITableView;
import net.ionoff.center.client.event.ShowLoadingEvent;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.sensor.SensorEditPresenter;
import net.ionoff.center.client.service.EntityService;
import net.ionoff.center.client.service.IRpcServiceProvider;
import net.ionoff.center.client.utils.ClientUtil;
import net.ionoff.center.shared.dto.*;
import org.fusesource.restygwt.client.Method;
import org.fusesource.restygwt.client.MethodCallback;

import java.util.List;

public class ScaleDataTablePresenter extends AbstractTablePresenter<SensorDataDto> {


    public interface Display extends ITableView<SensorDataDto> {
        Column<SensorDataDto, String> getNameColumn();
        ScaleDataEditPresenter.Display getScaleDataEditView();
    }

    private final Display view;
    private WeighScaleDto scaleDto;
    private ScaleDataEditPresenter dataEditPresenter;

    public ScaleDataTablePresenter(IRpcServiceProvider rpcProvider, HandlerManager eventBus,
                                   WeighScaleDto scaleDto, Display view) {
        super(rpcProvider, eventBus, view);
        setScale(scaleDto);
        this.view = view;
    }

    @Override
    public void bind() {
        super.bind();
        view.getEditColumn().setFieldUpdater(new FieldUpdater<SensorDataDto, String>() {
            @Override
            public void update(int index, SensorDataDto object, String value) {
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
    protected void save() {
        // does nothing
    }

    public void setScale(WeighScaleDto scaleDto) {
        this.scaleDto = scaleDto;
        display.getToolBarView().getLblTitle().setText(scaleDto.getName());
    }

    @Override
    protected boolean validateBeforeSaving() {
        return true;
    }

    @Override
    protected void add() {
        // does nothing
    }

    @Override
    protected AsyncDataProvider<SensorDataDto> newAsyncDataProvider() {
        final AsyncDataProvider<SensorDataDto> provider = new AsyncDataProvider<SensorDataDto>() {
            @Override
            protected void onRangeChanged(HasData<SensorDataDto> hasData) {
                final int start = hasData.getVisibleRange().getStart();
                // int length = hasData.getVisibleRange().getLength();
                loadData(null, false, start);
            }
        };
        return provider;
    }

    @Override
    protected void fillListBoxSearchBy() {
        display.getToolBarView().getLisBoxSearchBy().addItem(AdminLocale.getAdminConst().time());
    }

    @Override
    protected boolean isSameId(SensorDataDto entity, Long selectedId) {
        return entity.getId() == selectedId;
    }

    @Override
    protected String getClazz() {
        return SensorDataDto.class + "";
    }

    @Override
    protected String getSearchBy() {
        return "time";
    }

    @Override
    protected EntityService<SensorDataDto> getRpcService() {
        return rpcProvider.getSensorDataService();
    }

    @Override
    protected String getSortByField(int columnIndex) {
        return "time";
    }

    public ScaleDataEditPresenter getDataEditPresenter() {
        if (dataEditPresenter == null) {
            dataEditPresenter = new ScaleDataEditPresenter(rpcProvider, eventBus,
                    view.getScaleDataEditView(), this);
            dataEditPresenter.go();
        }
        return dataEditPresenter;
    }

    public void hideEditForm() {
        view.hideEditForm();
    }

    @Override
    protected void setSelectedObject(SensorDataDto selectedDto) {
        getDataEditPresenter().setEntityDto(selectedDto);
    }

    private void showEditForm() {
        view.showEditForm();
    }

    @Override
    protected String getKeySearch() {
        return display.getToolBarView().getDateBoxFromText() + " 00:00:00"
                + "-" + display.getToolBarView().getDateBoxToText() + " 23:59:59";
    }

    @Override
    protected QueryCriteriaDto buildSearchCriteria(int startIndex) {
        QueryCriteriaDto criteriaDto = super.buildSearchCriteria(startIndex);
        criteriaDto.setDeviceId(scaleDto.getId());
        return criteriaDto;
    }

    @Override
    protected QueryCriteriaDto buildCountCriteria() {
        QueryCriteriaDto criteriaDto = super.buildCountCriteria();
        criteriaDto.setDeviceId(scaleDto.getId());
        return criteriaDto;
    }
}