package net.ionoff.center.client.ui;

import com.google.gwt.user.client.ui.FlowPanel;
import com.google.gwt.user.client.ui.PopupPanel;
import gwt.material.design.client.constants.ButtonType;
import gwt.material.design.client.constants.IconType;
import gwt.material.design.client.constants.WavesType;
import gwt.material.design.client.ui.MaterialButton;
import gwt.material.design.client.ui.MaterialIcon;
import gwt.material.design.client.ui.MaterialLink;
import gwt.material.design.client.ui.MaterialTitle;
import net.ionoff.center.client.locale.AdminLocale;
import net.ionoff.center.client.locale.ProjectLocale;
import net.ionoff.center.client.navigation.ProjectMenuItemView;

import java.util.ArrayList;
import java.util.List;

public class PopupTableRowMenu extends PopupPanel {

    private final FlowPanel wrapper;
    private final FlowPanel contentWrapper;
    private final MaterialButton menuItemEdit;
    private final MaterialButton menuItemDelete;

    public PopupTableRowMenu() {
        super(true, true);
        setStyleName("popupTableRowMenu");
        setAnimationEnabled(true);
        setAnimationType(AnimationType.ROLL_DOWN);

        wrapper = new FlowPanel();
        setWidget(wrapper);

        contentWrapper = new FlowPanel();
        contentWrapper.addStyleName("wrapper");
        wrapper.add(contentWrapper);

        menuItemEdit = new MaterialButton(ButtonType.FLAT);
        menuItemEdit.addStyleName("menuItem");
        menuItemEdit.setIconType(IconType.EDIT);
        menuItemEdit.setText(ProjectLocale.getProjectConst().edit());
        menuItemEdit.setWaves(WavesType.YELLOW);
        contentWrapper.add(menuItemEdit);

        menuItemDelete = new MaterialButton(ButtonType.FLAT);
        menuItemDelete.setIconType(IconType.DELETE_FOREVER);
        menuItemDelete.addStyleName("menuItem");
        menuItemDelete.setText(ProjectLocale.getProjectConst().delete());
        menuItemDelete.setWaves(WavesType.RED);
        contentWrapper.add(menuItemDelete);
    }

    public FlowPanel getContentWrapper() {
        return contentWrapper;
    }

    public MaterialButton getMenuItemDelete() {
        return menuItemDelete;
    }

    public MaterialButton getMenuItemEdit() {
        return menuItemEdit;
    }
}