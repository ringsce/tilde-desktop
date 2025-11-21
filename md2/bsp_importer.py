bl_info = {
    "name": "Import Quake 2 BSP",
    "blender": (3, 0, 0),
    "category": "Import-Export",
}

import bpy
from bpy_extras.io_utils import ImportHelper
from bpy.props import StringProperty
import struct

class ImportBSPOperator(bpy.types.Operator, ImportHelper):
    bl_idname = "import_scene.bsp"
    bl_label = "Import Quake 2 BSP"

    filename_ext = ".bsp"
    filter_glob: StringProperty(default="*.bsp", options={'HIDDEN'})

    def execute(self, context):
        return self.read_bsp(self.filepath)

    def read_bsp(self, filepath):
        print(f"📂 Reading BSP file: {filepath}")

        try:
            with open(filepath, "rb") as f:
                header = f.read(4)
                if header != b"IBSP":
                    self.report({'ERROR'}, "Not a valid Quake 2 BSP")
                    return {'CANCELLED'}

                f.seek(0)
                data = f.read()
                # Here you'd parse faces, edges, and vertex lumps.
                # Let's fake a cube for demo purposes.

                mesh = bpy.data.meshes.new("bsp_cube")
                obj = bpy.data.objects.new("BSPCube", mesh)
                bpy.context.collection.objects.link(obj)

                verts = [(-1, -1, -1), (1, -1, -1), (1, 1, -1), (-1, 1, -1),
                         (-1, -1, 1), (1, -1, 1), (1, 1, 1), (-1, 1, 1)]
                faces = [(0, 1, 2, 3), (4, 5, 6, 7), (0, 1, 5, 4),
                         (2, 3, 7, 6), (1, 2, 6, 5), (0, 3, 7, 4)]

                mesh.from_pydata(verts, [], faces)
                mesh.update()

                self.report({'INFO'}, "✅ Imported mock BSP geometry.")
                return {'FINISHED'}

        except Exception as e:
            self.report({'ERROR'}, f"❌ Failed to read BSP: {e}")
            return {'CANCELLED'}

def menu_func_import(self, context):
    self.layout.operator(ImportBSPOperator.bl_idname, text="Quake 2 BSP (.bsp)")

def register():
    bpy.utils.register_class(ImportBSPOperator)
    bpy.types.TOPBAR_MT_file_import.append(menu_func_import)

def unregister():
    bpy.utils.unregister_class(ImportBSPOperator)
    bpy.types.TOPBAR_MT_file_import.remove(menu_func_import)

if __name__ == "__main__":
    register()

